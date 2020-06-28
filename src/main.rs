#![warn(missing_docs, clippy::missing_docs_in_private_items)]

//! # Simple MIDI sequencer
//!
//! A simplistic sequencer that acts on MIDI files. It supports "playing" the
//! sequence (actually just printing the sequence and parameter changes to
//! stdout) or writing to a MIDI file. You can choose from multiple voices,
//! with different sets of parameters, which are encoded using MIDI program
//! changes and MIDI CC for the parameters.
//!
//! The sequences can be arbitrary fractions of the host BPM, although if the
//! divisor isn't a multiple of 3 or 2 then it will probably slowly drift
//! because of inaccuracies in the division of `TICKS_PER_BEAT`. A real project
//! would prevent this at runtime by disallowing values for this BPM fraction
//! that would cause inaccuracies, or possibly by using floating-/fixed-point
//! maths along with some extra logic to keep the tracks in sync.
//!
//! ## Usage
//!
//! The input is just an enum with a `serde::Deserialize` impl, deserialized
//! with [RON](https://github.com/ron-rs/ron). So for a list of the available
//! commands, see the `Command` enum, and for an example of how to write that
//! on the command line, see the documentation for RON. For an example of how
//! to set parameters on the sequencer, see `filetests/simple.input`. When
//! you want to see the output, either use `Start(..)` to write the messages
//! to stdout for the given period of time, or use `StartWrite`, use some
//! commands (such as `Tick` or even `Start`) and then save an output MIDI
//! file using `EndWrite("/path/to/output")`. The syntax of RON is very
//! similar to that of Rust, so it should be pretty simple to understand.
//!
//! ### Some notes
//!
//! - The code is a little overengineered, as I had committed to being able to
//!   save MIDI files, to being able to edit the sequences on-the-fly, and to
//!   being able to step the sequencer by arbitrary numbers of ticks. These
//!   constraints are self-imposed, though, and in a real project you might be
//!   able to relax many of them in order to make the code a bit simpler.
//! - Relatedly, I've kept all the code in one file since (in my opinion) it makes
//!   it easier to review the whole project on GitHub and similar. It's still below
//!   1000SLC (much of the size of the file is comments and docs) so it's debatable
//!   whether or not it should be split into modules yet.
//! - I've chosen not to make it multi-threaded, so while you can edit sequences
//!   while they're playing, this is done by using a `Tick` function to progress
//!   time. In other words, it's asynchronous but not concurrent. I did this so
//!   that when operating it on the command line you don't have the input and output
//!   overwriting one another. The best way to make this multi-threaded would be to
//!   separate the parts of `Sequencer` and `Sequence` which are updated in
//!   response to user input but only read on tick, such as the sequences
//!   themselves, from the parts of `Sequencer` and `Sequence` which are unneeded
//!   when handling user input but written on tick, such as `Sequence::cur_note`.
//!   You could also just put the whole `Sequencer` in a `Mutex`, which is a lot
//!   easier and should do the job in most cases.
//! - In many places I've returned an `impl Iterator` instead of a collection,
//!   even when the iterator is just `arrayvec::IntoIter`. `impl Iterator` is
//!   the only form of `impl Trait` that I use by default, because with collections
//!   I've found it's very common to start by eagerly generating a collection for
//!   simplicity, and then go back later to make it a lazy iterator with `O(1)`
//!   space complexity.
//! - While it was mostly built with an embedded environment in mind, we need
//!   to allocate in order to save MIDI files (this isn't inherent in saving
//!   MIDI files, just an artifact of the library chosen). Additionally, we
//!   use floating-point maths in order to deal with seconds, but this is just
//!   for convenience and we could use fixed-point maths as long as we're happy
//!   to choose a resolution that makes sense for the project. I've also avoided
//!   `unwrap` except for the results of constant expressions and in `main`.
//! - I use MIDI as my interchange format, both internally and for writing to
//!   a file. MIDI is pretty limiting, and in a real project we'd either hard-
//!   code the sequencer to the synth without a MIDI-like interchange format
//!   or have a custom interchange format that is a superset of MIDI so that
//!   we can still output to MIDI and receive MIDI input, just with fewer
//!   features than when you connect the sequencer to the synth directly.
//! - Relatedly, I use `ghakuf` as my MIDI library, basically just because it's
//!   the first library that I found that supports writing MIDI files. I wouldn't
//!   use this in a real project because it requires allocation and doesn't
//!   support streaming serialisation of MIDI files. In fact, its method of
//!   writing MIDI files is really weird, where it has an internal
//!   `Vec<&'a Message>` instead of `Vec<Message>`, forcing you to have two
//!   buffers, one for the owned data, and one inside `ghakuf` of references to
//!   that owned data.

// `arraytools` is used for mapping fixed-size arrays to fixed-size arrays. This
// isn't really necessary, we could use arrayvec. I chose to map fixed-size arrays
// to fixed-size arrays instead of separately storing the length because in my
// opinion it expresses intent better, but this is subjective and would be something
// that would need to be discussed with the rest of the team in code review.
use arraytools::ArrayTools;
// `arrayvec` is used to get dynamically-sized arrays with a fixed upper bound on
// size. One caveat is that if you use this library and your _actual_ upper bound
// on size is larger than the one in the `arrayvec`, then functions like `collect`
// will silently drop data, so you should be careful when using it for user input.
// We only use it in this project for data that we control that has a strict upper
// bound, though.
use arrayvec::ArrayVec;
use ghakuf::messages::{Message, MetaEvent, MidiEvent};
// We could just inline the definition of `div_ceil` into our crate, since it's so
// simple, but this means we get it on any integer type and as a method (plus the
// `div_floor` method, which is the same as integer division but documents our
// intent better).
use num_integer::Integer;
use serde::Deserialize;
use std::{
    io::{self, BufRead},
    iter, mem,
    num::{NonZeroU64, NonZeroU8},
    str,
};

/// A channel ID, an index for a sequencer track between 0..NUM_CHANNELS
type Channel = u8;
/// A note ID, as defined by the MIDI spec
type Note = u8;

/// A synth voice for a track, with its associated parameters. This is just
/// an example to show how setting parameters and changing the voice would
/// work, so I haven't defined, for example, what the mapping from a byte of
/// `filter_freq` to a specific frequency in Hz might be. In a real program
/// you might want to avoid MIDI entirely just because of the restriction of
/// CCs to 8 bits, but I wanted the sequencer to just work with MIDI so I've
/// just dealt with that restriction for now.
// NOTE: I don't derive `Copy` because it can cause footguns with `&mut` methods,
//       especially when a variable is captured by a closure. It can be unclear
//       for `Copy` types if a mutation is only for a single invokation of the
//       closure or will persist to later invokations.
#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
enum Voice {
    /// Fake parameters for a theoretical clap synth
    Clap {
        /// Number of peaks in the envelope
        envelope_peaks: u8,
        /// Amount of time between each peak in the envelope
        envelope_peak_dist: u8,
        /// Low-pass filter frequency
        filter_freq: u8,
    },
    /// Fake parameters for a theoretical kick drum sampler or wave table synth
    SampledKick {
        /// Index of the sample (or possibly the index of the wave table)
        sample_index: u8,
        /// Low-pass filter frequency
        filter_freq: u8,
    },
    /// Fake parameters for a theoretical FM synth
    FM {
        /// The frequency of the carrier (as mentioned above, defining the meaning
        /// of this u8 or implementing a wider range of values for frequency is out
        /// of scope for this project)
        carrier_freq: u8,
        /// The frequency of the modulator
        modulator_freq: u8,
        /// Low-pass filter frequency
        lowpass_filter_freq: u8,
        /// High-pass filter frequency
        highpass_filter_freq: u8,
    },
}

/// ID for the "clap" voice, for emitting a "program change" MIDI message
const CLAP_PROGRAM: u8 = 0;
/// ID for the "sampled kick" voice, for emitting a "program change" MIDI message
const SAMPLED_KICK_PROGRAM: u8 = 1;
/// ID for the "FM synth" voice, for emitting a "program change" MIDI message
const FM_PROGRAM: u8 = 2;

/// CC index for the low-pass frequency parameter
const FILTER_FREQ_CC: u8 = 16;
/// CC index for the envelope peaks parameter
const ENVELOPE_PEAKS_CC: u8 = 17;
/// CC index for the envelope peak distance parameter
const ENVELOPE_PEAK_DIST_CC: u8 = 18;
/// CC index for the sample index parameter
const SAMPLE_INDEX_CC: u8 = 19;
/// CC index for the carrier frequency parameter
const CARRIER_FREQ_CC: u8 = 20;
/// CC index for the modulator frequency parameter
const MODULATOR_FREQ_CC: u8 = 21;
/// CC index for the high-pass frequency parameter
const HIGHPASS_FILTER_FREQ_CC: u8 = 22;
/// CC index for the FM synth's low-pass frequency parameter (I've
/// just made this overlap with the low-pass filter for the other
/// voices)
const LOWPASS_FILTER_FREQ_CC: u8 = FILTER_FREQ_CC;

impl Default for Voice {
    fn default() -> Self {
        Voice::FM {
            carrier_freq: u8::MAX / 2,
            modulator_freq: u8::MAX / 2,
            lowpass_filter_freq: u8::MAX,
            highpass_filter_freq: 0,
        }
    }
}

// I prefix the constants in the matches with `self::` so that if there's a typo
// in the name of the constant it doesn't get treated as a variable and so match
// anything. Warnings about naming and unused variables will be thrown by the compiler,
// so as long as you fix those warnings you won't have an issue, but it's a footgun
// that I'd rather avoid entirely.
impl Voice {
    /// Get the program ID for this voice (for the "program change" MIDI message)
    fn program(&self) -> u8 {
        match self {
            Self::Clap { .. } => CLAP_PROGRAM,
            Self::SampledKick { .. } => SAMPLED_KICK_PROGRAM,
            Self::FM { .. } => FM_PROGRAM,
        }
    }

    /// Get the MIDI events needed to initialise this voice - i.e. change the program,
    /// set the various CC parameters.
    fn init(&self, channel: u8) -> impl Iterator<Item = MidiEvent> {
        let mut vec = ArrayVec::<[_; 5]>::new();

        vec.push(MidiEvent::ProgramChange {
            ch: channel,
            program: self.program(),
        });

        match *self {
            Voice::Clap {
                envelope_peaks: new_envelope_peaks,
                envelope_peak_dist: new_envelope_peak_dist,
                filter_freq: new_filter_freq,
            } => {
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: ENVELOPE_PEAKS_CC,
                    data: new_envelope_peaks,
                });
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: ENVELOPE_PEAK_DIST_CC,
                    data: new_envelope_peak_dist,
                });
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: FILTER_FREQ_CC,
                    data: new_filter_freq,
                });
            }
            Voice::SampledKick {
                sample_index: new_sample_index,
                filter_freq: new_filter_freq,
            } => {
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: SAMPLE_INDEX_CC,
                    data: new_sample_index,
                });
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: FILTER_FREQ_CC,
                    data: new_filter_freq,
                });
            }
            Voice::FM {
                carrier_freq: new_carrier_freq,
                modulator_freq: new_modulator_freq,
                lowpass_filter_freq: new_lowpass_filter_freq,
                highpass_filter_freq: new_highpass_filter_freq,
            } => {
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: CARRIER_FREQ_CC,
                    data: new_carrier_freq,
                });
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: MODULATOR_FREQ_CC,
                    data: new_modulator_freq,
                });
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: LOWPASS_FILTER_FREQ_CC,
                    data: new_lowpass_filter_freq,
                });
                vec.push(MidiEvent::ControlChange {
                    ch: channel,
                    control: HIGHPASS_FILTER_FREQ_CC,
                    data: new_highpass_filter_freq,
                });
            }
        }

        vec.into_iter()
    }

    /// Get an iterator of MIDI CCs to emit in order to change from the old voice to
    /// the new voice.
    fn diff(old: &Self, new: &Self, channel: u8) -> impl Iterator<Item = MidiEvent> {
        let mut vec = ArrayVec::<[_; 5]>::new();

        match (old, new) {
            (
                &Voice::Clap {
                    envelope_peaks: old_envelope_peaks,
                    envelope_peak_dist: old_envelope_peak_dist,
                    filter_freq: old_filter_freq,
                },
                &Voice::Clap {
                    envelope_peaks: new_envelope_peaks,
                    envelope_peak_dist: new_envelope_peak_dist,
                    filter_freq: new_filter_freq,
                },
            ) => {
                if old_envelope_peaks != new_envelope_peaks {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: ENVELOPE_PEAKS_CC,
                        data: new_envelope_peaks,
                    });
                }
                if old_envelope_peak_dist != new_envelope_peak_dist {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: ENVELOPE_PEAK_DIST_CC,
                        data: new_envelope_peak_dist,
                    });
                }
                if old_filter_freq != new_filter_freq {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: FILTER_FREQ_CC,
                        data: new_filter_freq,
                    });
                }
            }
            (
                &Voice::SampledKick {
                    sample_index: old_sample_index,
                    filter_freq: old_filter_freq,
                },
                &Voice::SampledKick {
                    sample_index: new_sample_index,
                    filter_freq: new_filter_freq,
                },
            ) => {
                if old_sample_index != new_sample_index {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: SAMPLE_INDEX_CC,
                        data: new_sample_index,
                    });
                }
                if old_filter_freq != new_filter_freq {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: FILTER_FREQ_CC,
                        data: new_filter_freq,
                    });
                }
            }
            (
                &Voice::FM {
                    carrier_freq: old_carrier_freq,
                    modulator_freq: old_modulator_freq,
                    lowpass_filter_freq: old_lowpass_filter_freq,
                    highpass_filter_freq: old_highpass_filter_freq,
                },
                &Voice::FM {
                    carrier_freq: new_carrier_freq,
                    modulator_freq: new_modulator_freq,
                    lowpass_filter_freq: new_lowpass_filter_freq,
                    highpass_filter_freq: new_highpass_filter_freq,
                },
            ) => {
                if old_carrier_freq != new_carrier_freq {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: CARRIER_FREQ_CC,
                        data: new_carrier_freq,
                    });
                }
                if old_modulator_freq != new_modulator_freq {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: MODULATOR_FREQ_CC,
                        data: new_modulator_freq,
                    });
                }
                if old_lowpass_filter_freq != new_lowpass_filter_freq {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: LOWPASS_FILTER_FREQ_CC,
                        data: new_lowpass_filter_freq,
                    });
                }
                if old_highpass_filter_freq != new_highpass_filter_freq {
                    vec.push(MidiEvent::ControlChange {
                        ch: channel,
                        control: HIGHPASS_FILTER_FREQ_CC,
                        data: new_highpass_filter_freq,
                    });
                }
            }
            // If the voices are different, just initialise a new voice.
            _ => {
                vec.extend(new.init(channel));
            }
        }

        vec.into_iter()
    }
}

/// A single note in the sequence.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct SequenceNote {
    /// The note ID
    note: Note,
    /// The note's velocity
    velocity: u8,
    /// The maximum length of the note, or infinite if `None`.
    ///
    /// Currently if you set a duration in seconds it will convert it to ticks, so if the BPM
    /// changes later then the length of the note will change too. This is arguably fine for
    /// an MVP but in a real project you'd make `duration` an enum that can either be a number
    /// of ticks or a number of seconds.
    duration: Option<NonZeroU64>,
}

/// A sequence parameter to set
#[derive(Deserialize)]
enum Param {
    /// Set the number of steps in the sequence before it loops.
    NumSteps(u8),
    /// Set the step length multiplier. By default, 1 step is 1 beat. This
    /// changes the length of notes as well as the distance between them.
    Multiplier(NonZeroU8, NonZeroU8),
}

/// A command to send to the sequencer from the command line. We just parse this using `serde` and
/// RON, instead of inventing our own command language.
#[derive(Deserialize)]
enum Command {
    /// Play the sequencer for some amount of beats or seconds, displaying the output in real-time on
    /// stdout.
    Start(Time),
    /// Reset the sequencer (put it back to the start)
    Reset,
    /// Clear all the sequences and reset the sequencer to its default state
    Clear,
    /// Begin writing a MIDI file. Every time `Tick` (or `Start`) is used, the resulting MIDI messages
    /// are written to an internal buffer, which are then serialised to disk when `EndWrite` is used.
    ///
    /// If this is called a second time, it will discard the existing write session and begin a new
    /// clean one
    StartWrite,
    /// Serialise the set of MIDI events recorded since the last use of `StartWrite` to disk, at the
    /// provided path.
    EndWrite(std::path::PathBuf),
    /// Advance the sequencer forward some number of beats or seconds immediately, adding the generated
    /// MIDI messages to the output MIDI file if `StartWrite` was called.
    Tick(Time),
    /// Set the master BPM for the sequencer, although individual tracks can have separate BPMs as
    /// long as they are a whole fraction of the master BPM.
    SetBpm(u32),
    /// Set a parameter of a channel (see the documentation for `Param` for the available parameters)
    SetParam {
        /// The channel to affect
        channel: Channel,
        /// The parameter (see the documentation for `Param`)
        param: Param,
    },
    /// Insert a note at the given index in the sequence for the given channel
    Insert {
        /// The channel to affect
        channel: Channel,
        /// The MIDI note ID (middle C is 60, going up or down by 1 changes the
        /// note by a single semitone)
        note: Note,
        /// The note's velocity
        velocity: u8,
        /// The index in the sequence
        index: u8,
        /// How long the note should last (see documentation for `Time`)
        duration: Time,
    },
    /// Insert a change to the track's voice at the given index in the sequence
    /// for the given channel.
    InsertVoiceChange {
        /// The channel to affect
        channel: Channel,
        /// The index in the sequence
        index: u8,
        /// The new voice to use, with its parameters. I don't have a Elektron-style system
        /// of a "default" track-wide voice with each sequence step being able to change just
        /// one of those parameters, voices are tied to steps in the sequence alone.
        voice: Voice,
    },
    /// Remove any note and voice change at the given index
    Remove {
        /// The channel to affect
        channel: Channel,
        /// The index in the sequence
        index: u8,
    },
}

/// A duration, relative to the current BPM. Internally this is converted to ticks, and so therefore
/// if you set a note's duration in terms of seconds and then change the BPM, it will change length.
#[derive(Deserialize)]
enum Time {
    /// A number of seconds, as a floating-point number.
    ///
    /// In a real embedded project this should probably be a fixed-point number. There are good
    /// libraries for fixed-point maths in Rust but for a desktop application floating-point is
    /// good enough.
    Seconds(f64),
    /// A number of beats, as a fraction (so `Beats(1, 4)` is 1 quarter-beat)
    Beats(u8, u8),
    /// Infinite time.
    Infinite,
}

/// The number of channels in the sequencer
const NUM_CHANNELS: usize = 4;
/// The maximum size of a sequence, in steps. This doesn't affect the maximum length of a sequence in
/// beats, as you can set the multiplier to be anything up to 255.
///
/// Indices are assumed to be 8 bits, so this cannot be higher than 256
const MAX_SEQUENCE_SIZE: usize = 64;
/// The default size of a sequence, in steps.
const DEFAULT_SEQUENCE_SIZE: usize = 4;

/// A fraction that cannot be x/0 or 0/x. It might be ultimately better to use `u8` but treat each value
/// as that value plus 1, so we can multiply or divide by numbers up to 256, but that's out of scope for
/// this project.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Fraction {
    /// The amount to multiply by
    numerator: NonZeroU8,
    /// The amount to divide by
    divisor: NonZeroU8,
}

impl Default for Fraction {
    fn default() -> Self {
        Fraction {
            numerator: NonZeroU8::new(1).unwrap(),
            divisor: NonZeroU8::new(1).unwrap(),
        }
    }
}

/// An individual track in the sequencer, defined by a set of notes, a set of parameter changes, and
/// a voice.
#[derive(Debug)]
struct Sequence {
    /// The step length multiplier, so `2/1` means each step takes 2 beats. Expect this to have
    /// weird behaviour if the divisor doesn't neatly go into `TICKS_PER_BEAT`
    multiplier: Fraction,
    /// The notes and voice parameter changes in the sequence.
    steps: ArrayVec<[(Option<SequenceNote>, Option<Voice>); MAX_SEQUENCE_SIZE]>,
    /// The current voice of this track
    voice: Voice,
    /// The last time that this sequence repeated. This is avoid dividing the current global tick
    /// in order to work out which step in the sequence we should be in, which would make the
    /// position in the sequence vary wildly when changing the sequence size or multiplier.
    ///
    /// However, this _does_ make it so within a single play of the sequence changing the
    /// length/multiplier will change our position in the sequence, which is deliberate but it's
    /// debatable which behaviour is preferable.
    last_start_tick: u64,
    /// We duplicate the action here, which means you can't change the length of a note once
    /// it's been played.
    cur_note: Option<(u64, SequenceNote)>,
}

impl Sequence {
    /// Create a new sequence with the specified voice
    fn new(voice: Voice) -> Self {
        Sequence {
            multiplier: Fraction::default(),
            voice,
            steps: iter::repeat((None, None))
                .take(DEFAULT_SEQUENCE_SIZE)
                .collect(),
            last_start_tick: 0,
            cur_note: None,
        }
    }

    /// Set the number of steps in the sequence
    fn set_num_steps(&mut self, num_steps: usize) {
        self.steps
            .extend(iter::repeat((None, None)).take(num_steps.saturating_sub(self.steps.len())));
        self.steps.truncate(num_steps);
    }

    /// Sets how long each step is, in beats. `Fraction { numerator: 2, divisor: 3 }` will make each
    /// step take 2/3 beats. Since we do this with integer arithmetic this is likely to be somewhat
    /// unreliable, we should preserve as much information as possible for as long as possible. In
    /// this case that would mean duplicating the multiplication in multiple places instead of
    /// just calculating `ticks_per_beat` once in `events_between`.
    fn set_multiplier(&mut self, multiplier: Fraction) {
        self.multiplier = multiplier;
    }

    /// Get the MIDI events between the two specified ticks. We take a "channel" argument instead of
    /// storing the channel on the sequence itself to avoid duplicating information.
    fn events_between(
        &mut self,
        channel: Channel,
        start: u64,
        end: u64,
    ) -> impl Iterator<Item = (u64, MidiEvent)> + '_ {
        let Self {
            steps,
            voice,
            cur_note,
            last_start_tick,
            multiplier,
            ..
        } = self;

        let ticks_per_beat =
            (TICKS_PER_BEAT * multiplier.numerator.get() as u64) / multiplier.divisor.get() as u64;

        // So that we don't have surprising behaviour if the sequence length changes
        // between calls to `tick`, we store the last time that the sequence looped.
        // This means that we don't calculate our current beat position relative to
        // when the track originally started, which would lead to our position jumping
        // around all over the place when the length of the sequence is changed.
        let start_tick = *last_start_tick;

        let ticks_per_sequence = ticks_per_beat * steps.len() as u64;

        while ticks_per_sequence != 0 && end >= *last_start_tick + ticks_per_sequence {
            *last_start_tick += ticks_per_sequence;
        }

        let first_index = (start - start_tick).div_ceil(&ticks_per_beat);

        // This gives us an iterator which goes through the sequence's steps, repeating, along with the
        // index. The index continues going up after each cycle (so if there was 4 elements, the second
        // time we see the first element it will now have index 4). Essentially, this gives us the beat
        // index of the current element, from `last_start_tick`.
        let mut steps_iter = steps.iter().cycle().enumerate().skip(first_index as usize);

        iter::from_fn(move || {
            let (index, (step_note, voice_change)) = steps_iter.next()?;
            let next_tick = start_tick + ticks_per_beat * index as u64;

            if next_tick >= end {
                return None;
            }

            let voice_change_events = voice_change
                .as_ref()
                .map(|voice_change| Voice::diff(voice, voice_change, channel));

            let note_on_events = step_note.as_ref().map(|step_note| MidiEvent::NoteOn {
                ch: channel,
                note: step_note.note,
                velocity: step_note.velocity,
            });

            let note_off_events = cur_note.clone().and_then(
                |(
                    started_tick,
                    SequenceNote {
                        note,
                        velocity,
                        duration,
                        ..
                    },
                )| {
                    let end_tick = duration.map(|duration| started_tick + duration.get());
                    let tick = match (step_note, end_tick) {
                        (None, Some(end_tick)) if end_tick <= next_tick => end_tick,
                        (Some(_), end_tick) => next_tick.min(end_tick.unwrap_or_default()),
                        _ => return None,
                    };

                    *cur_note = None;

                    Some((
                        tick,
                        MidiEvent::NoteOff {
                            ch: channel,
                            note,
                            velocity,
                        },
                    ))
                },
            );

            if let Some(step_note) = step_note {
                *cur_note = Some((next_tick, *step_note));
            }

            if let Some(voice_change) = voice_change {
                *voice = voice_change.clone();
            }

            Some((
                next_tick,
                voice_change_events,
                note_off_events,
                note_on_events,
            ))
        })
        .fuse()
        .flat_map(
            |(next_tick, voice_change_events, note_off_events, note_on_events)| {
                // We do `note_off`, followed by `voice_change`, followed by `note_on`.
                // `note_off` has to go first since a note can end either before or at the
                // same time as when the next note starts, but `voice_change` and `note_on`
                // can be in either order because they happen simultaneously.
                note_off_events.into_iter().chain(
                    voice_change_events
                        .into_iter()
                        .flatten()
                        .chain(note_on_events)
                        .map(move |event| (next_tick, event)),
                )
            },
        )
    }

    /// Pause or end playback. This will end any notes that are currently playing, unless
    /// their duration is set to infinite.
    fn finish(&mut self, channel: Channel, end: Option<u64>) -> Option<(u64, MidiEvent)> {
        self.cur_note.take().and_then(
            |(
                started_tick,
                SequenceNote {
                    note,
                    velocity,
                    duration,
                    ..
                },
            )| {
                let tick = match (end, duration) {
                    (Some(end), Some(duration)) => end.min(started_tick + duration.get()),
                    (None, Some(duration)) => started_tick + duration.get(),
                    (Some(end), None) => end,
                    (None, None) => return None,
                };

                Some((
                    tick,
                    MidiEvent::NoteOff {
                        ch: channel,
                        note,
                        velocity,
                    },
                ))
            },
        )
    }
}

/// For printing the output to stdout, converts an internally-generated MIDI message to a form
/// which matches the human-readable input better.
fn print_midi_message(message: &Message) {
    match message {
        Message::MetaEvent {
            event: MetaEvent::SetTempo,
            data,
            ..
        } => {
            let tempo = (data[0] as u32) << 16 | (data[1] as u32) << 8 | data[2] as u32;
            let bpm: u32 = 60 * 1000000 / tempo;

            println!("BPM: {}", bpm);
        }
        &Message::MidiEvent {
            event: MidiEvent::ProgramChange { ch, program },
            ..
        } => {
            println!(
                "track {} set instrument: {}",
                ch,
                match program {
                    self::CLAP_PROGRAM => "clap",
                    self::SAMPLED_KICK_PROGRAM => "kick",
                    self::FM_PROGRAM => "fm synth",
                    _ => return,
                }
            );
        }
        &Message::MidiEvent {
            event: MidiEvent::ControlChange { ch, control, data },
            ..
        } => println!(
            "track {} set {}: {}",
            ch,
            match control {
                self::FILTER_FREQ_CC => "filter freq",
                self::SAMPLE_INDEX_CC => "sample index",
                self::ENVELOPE_PEAKS_CC => "envelope peaks",
                self::ENVELOPE_PEAK_DIST_CC => "envelope peak dist",
                self::CARRIER_FREQ_CC => "carrier freq",
                self::MODULATOR_FREQ_CC => "modulator freq",
                self::HIGHPASS_FILTER_FREQ_CC => "highpass filter freq",
                _ => return,
            },
            data
        ),
        &Message::MidiEvent {
            event: MidiEvent::NoteOn { ch, note, velocity },
            ..
        } => println!("track {} press {} ({})", ch, note, velocity),
        &Message::MidiEvent {
            event: MidiEvent::NoteOff { ch, note, velocity },
            ..
        } => println!("track {} release {} ({})", ch, note, velocity),
        _ => {}
    }
}

impl Default for Sequence {
    fn default() -> Self {
        Self::new(Voice::default())
    }
}

/// The SMF time base value. 48 is the default, I believe, but it's important to choose a
/// value like 48 or 60 which can be divided cleanly by many different numbers.
const TICKS_PER_QUARTER_BEAT: u64 = 48;
/// This essentially defines the minimum length of a note. The maximum length of a note
/// is `u8::MAX` (i.e. the maximum value of `Fraction::numerator`) multiplied by
/// `TICKS_PER_BEAT`. In order to prevent overflow, we could wrap the `tick` counter every
/// `TICKS_PER_BEAT * u8::MAX * MAX_SEQUENCE_SIZE` ticks but this would complicate the code
/// so for now we just make ticks a `u64` so we don't need to care about overflow.
///
/// We could even choose values of `TICKS_PER_BEAT` and `MAX_SEQUENCE_SIZE` that multiply
/// to get a power of 2 (such as 65536) so we can just use wrapping to get the modulo
/// behaviour that we want.
const TICKS_PER_BEAT: u64 = TICKS_PER_QUARTER_BEAT * 4;

/// The root sequencer type, which contains the global state as well as a number of channels.
struct Sequencer {
    /// The tracks in this sequencer
    channels: [Sequence; NUM_CHANNELS],
    /// The global tick count
    tick: u64,
    /// The global BPM
    bpm: u32,
    /// The tick of the last emitted message (since MIDI is based on delta time, not absolute
    /// time)
    last_message_tick: u64,
    /// The `ghakuf` library has no way to incrementally stream to an output file,
    /// so we need to build our own buffer, copy it to their buffer, and then copy
    /// that to the output file. This should be trivially fixable but it's out
    /// of scope for this project.
    ///
    /// This is the only place where we allocate, however.
    pending_messages: Option<Vec<Message>>,
}

impl Default for Sequencer {
    fn default() -> Self {
        Sequencer {
            channels: Default::default(),
            tick: 0,
            bpm: 120,
            last_message_tick: 0,
            pending_messages: None,
        }
    }
}

/// Error type when passing commands to the sequencer
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Error {
    /// The supplied channel is greater than `NUM_CHANNELS`
    ChannelDoesNotExist(u8),
    /// The supplied index is greater than the sequence size
    IndexOutOfBounds(u8),
    /// `EndWrite` was called before `StartWrite`
    NoWriteInProgress,
    /// The time passed was infinite when it should be finite
    InvalidTime,
    /// Writing the output MIDI file failed
    WriteFailed,
}

impl Sequencer {
    /// Convert a human-readble `Time` value to a possibly-null number of ticks. `None` represents an
    /// infinite time.
    fn to_ticks(&self, time: Time) -> Option<NonZeroU64> {
        match time {
            Time::Seconds(seconds) => {
                let out = seconds * TICKS_PER_BEAT as f64 * self.bpm as f64 / 60.;

                if out.is_normal() {
                    NonZeroU64::new(out as u64)
                } else {
                    None
                }
            }
            Time::Beats(numerator, divisor) => {
                NonZeroU64::new(TICKS_PER_BEAT * numerator as u64 / divisor as u64)
            }
            Time::Infinite => None,
        }
    }

    /// Receive a single command. See the documentation for `Command` for more information.
    fn update(&mut self, message: Command) -> Result<(), Error> {
        match message {
            Command::Start(time) => {
                use std::{thread, time::Duration};

                let ticks = self.to_ticks(time).ok_or(Error::InvalidTime)?;

                // We do `take` here so we can call `.tick` without causing lifetime errors,
                // since `tick` might modify `self.pending_messages`.
                let mut messages = self.pending_messages.take();

                let bpm = self.bpm;
                let to_seconds =
                    move |ticks: u64| ticks as f64 * 60. / (TICKS_PER_BEAT as f64 * bpm as f64);

                let &Self {
                    tick,
                    last_message_tick,
                    ..
                } = &*self;
                // We produce MIDI messages with a `delta_time` based on the last message,
                // but when printing to stdout we want the first message to be delayed
                // based on where the `tick` is, rather than `last_message_tick`. For
                // example, if you had a repeating pattern like `(note) (empty) (empty) (empty)`
                // then if you did `Start(Beats(4, 1))` twice in a row, if you didn't offset the
                // time like this then the second call to `Start` would essentially repeat the 3
                // empty messages again.
                let mut diff = tick.saturating_sub(last_message_tick);

                let out = self.tick(ticks.get());

                let mut total_waited_ticks = 0;

                for msg in out {
                    if let Some(messages) = &mut messages {
                        messages.push(msg.clone());
                    }

                    let wait_ticks = match msg {
                        Message::MetaEvent { delta_time, .. }
                        | Message::MidiEvent { delta_time, .. } => delta_time as u64,
                        _ => continue,
                    };
                    let wait_ticks = wait_ticks - diff;
                    diff = 0;

                    thread::sleep(Duration::from_secs_f64(to_seconds(wait_ticks)));

                    total_waited_ticks += wait_ticks;
                    print_midi_message(&msg);
                }

                thread::sleep(Duration::from_secs_f64(to_seconds(
                    ticks.get().saturating_sub(total_waited_ticks),
                )));

                self.pending_messages = messages;
            }
            Command::Reset => {
                self.tick = 0;
                self.last_message_tick = 0;

                for channel in &mut self.channels {
                    channel.last_start_tick = 0;
                }
            }
            Command::Tick(time) => {
                let ticks = self.to_ticks(time).ok_or(Error::InvalidTime)?;

                // We do `take` here so we can call `.tick` without causing lifetime errors,
                // since `tick` might modify `self.pending_messages`.
                let messages = self.pending_messages.take();

                let out = self.tick(ticks.get()).collect::<Vec<_>>();

                if let Some(mut messages) = messages {
                    messages.extend(out);
                    self.pending_messages = Some(messages);
                } else {
                    // We update some state in `fn tick`, so we consume the iterator here.
                    for _ in out {}
                }
            }
            Command::Clear => {
                *self = Self::default();
            }
            Command::SetBpm(bpm) => {
                self.bpm = bpm;

                if let Some(messages) = &mut self.pending_messages {
                    let tempo: u32 = 60 * 1000000 / self.bpm;

                    messages.push(Message::MetaEvent {
                        delta_time: 0,
                        event: MetaEvent::SetTempo,
                        data: [(tempo >> 16) as u8, (tempo >> 8) as u8, tempo as u8].to_vec(),
                    });
                }
            }
            Command::SetParam { channel, param } => {
                let channel = self
                    .channels
                    .get_mut(channel as usize)
                    .ok_or(Error::ChannelDoesNotExist(channel))?;

                match param {
                    Param::NumSteps(steps) => channel.set_num_steps(steps as _),
                    Param::Multiplier(numerator, divisor) => {
                        channel.set_multiplier(Fraction { numerator, divisor })
                    }
                }
            }
            Command::Insert {
                channel,
                note,
                velocity,
                index,
                duration,
            } => {
                let duration = self.to_ticks(duration);
                let channel = self
                    .channels
                    .get_mut(channel as usize)
                    .ok_or(Error::ChannelDoesNotExist(channel))?;

                let (step_note, _) = channel
                    .steps
                    .get_mut(index as usize)
                    .ok_or(Error::IndexOutOfBounds(index))?;

                *step_note = Some(SequenceNote {
                    note,
                    velocity,
                    duration,
                });
            }
            Command::InsertVoiceChange {
                channel,
                index,
                voice,
            } => {
                let channel = self
                    .channels
                    .get_mut(channel as usize)
                    .ok_or(Error::ChannelDoesNotExist(channel))?;

                let (_, step_params) = channel
                    .steps
                    .get_mut(index as usize)
                    .ok_or(Error::IndexOutOfBounds(index))?;

                *step_params = Some(voice);
            }
            Command::Remove { channel, index } => {
                let channel = self
                    .channels
                    .get_mut(channel as usize)
                    .ok_or(Error::ChannelDoesNotExist(channel))?;

                mem::take(
                    channel
                        .steps
                        .get_mut(index as usize)
                        .ok_or(Error::IndexOutOfBounds(index))?,
                );
            }
            Command::StartWrite => {
                let tempo: u32 = 60 * 1000000 / self.bpm;
                let mut pending = vec![Message::MetaEvent {
                    delta_time: 0,
                    event: MetaEvent::SetTempo,
                    data: [(tempo >> 16) as u8, (tempo >> 8) as u8, tempo as u8].to_vec(),
                }];

                for (ch_id, channel) in self.channels.iter().enumerate() {
                    pending.extend(channel.voice.init(ch_id as u8).map(|event| {
                        Message::MidiEvent {
                            delta_time: 0,
                            event,
                        }
                    }));
                }

                self.pending_messages = Some(pending);
            }
            Command::EndWrite(path) => {
                let mut messages = self
                    .pending_messages
                    .take()
                    .ok_or(Error::NoWriteInProgress)?;
                let tick = self.tick;
                let mut end_messages = ArrayVec::from(self.channels.as_mut_array())
                    .into_iter()
                    .enumerate()
                    .filter_map(|(ch, seq)| seq.finish(ch as u8, Some(tick)))
                    .collect::<ArrayVec<[_; NUM_CHANNELS]>>();

                end_messages.sort_unstable_by_key(|&(tick, _)| tick);

                messages.extend(end_messages.into_iter().map(|(tick, event)| {
                    let dt = (tick - self.last_message_tick) as u32;
                    self.last_message_tick = tick;

                    Message::MidiEvent {
                        delta_time: dt,
                        event,
                    }
                }));
                messages.push(Message::MetaEvent {
                    delta_time: (self.tick - self.last_message_tick) as u32,
                    event: MetaEvent::EndOfTrack,
                    data: vec![],
                });

                let mut writer = ghakuf::writer::Writer::new();

                writer.time_base(TICKS_PER_QUARTER_BEAT as _);

                for msg in &messages {
                    writer.push(&msg);
                }

                writer.write(&path).map_err(|_| Error::WriteFailed)?;
            }
        }

        Ok(())
    }

    /// Advance the sequencer by a given number of MIDI ticks. The iterator returned must be
    /// consumed in order to update the state, but Rust will warn us about this since `Iterator`
    /// is marked `#[must_use]`.
    fn tick(&mut self, num_ticks: u64) -> impl Iterator<Item = Message> + '_ {
        let start = self.tick;
        let end = self.tick + num_ticks;
        self.tick = end;

        let Sequencer {
            channels,
            last_message_tick,
            ..
        } = self;

        // We don't really need to use `arraytools` here, we can use `ArrayVec` or even `Vec`,
        // I chose the `arraytools` functions because they (in my opinion) best expess my
        // intent, but in a real application we should take other things into account such as
        // how mature the library is.
        let mut inner_messages = [0, 1, 2, 3]
            .zip(channels.as_mut_array())
            .map(move |(i, channel)| channel.events_between(i as u8, start, end));

        let mut messages = inner_messages.as_mut_array().map(|iter| iter.next());

        iter::from_fn(move || {
            for (cur, iter) in messages.iter_mut().zip(inner_messages.iter_mut()) {
                if cur.is_none() {
                    *cur = iter.next();
                }
            }

            let mut soonest = &mut None;
            for opt_msg in &mut messages {
                match (&soonest, &*opt_msg) {
                    (Some((cur_min, _)), Some((new_tick, _))) if cur_min < new_tick => continue,
                    (_, None) => continue,
                    _ => {}
                }

                soonest = opt_msg;
            }

            let (tick, event) = soonest.take()?;
            let dt = (tick - *last_message_tick) as u32;
            *last_message_tick = tick;

            Some(Message::MidiEvent {
                delta_time: dt,
                event,
            })
        })
        .fuse()
    }
}

fn main() {
    let mut sequencer = Sequencer::default();

    let stdin = io::stdin();
    let stdin = stdin.lock();

    for line in stdin.lines() {
        let line = line.unwrap();

        let cmd = match ron::from_str(&line) {
            Ok(cmd) => cmd,
            Err(e) => {
                eprintln!("{}", e);
                continue;
            }
        };

        match sequencer.update(cmd) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("{:?}", e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Command, Sequencer};

    // We can't generate MIDI to a byte buffer because of limitations in `ghakuf` so instead
    // we generate the files in `OUT_DIR` and compare them.
    #[test]
    fn filetests() {
        use std::{
            ffi::OsStr,
            fs::{self, File},
            io::Read,
            path::PathBuf,
        };

        let mut filetest_count = 0;

        let filetest_dir = PathBuf::from(concat!(env!("CARGO_MANIFEST_DIR"), "/filetests"));
        let output_dir = filetest_dir.join("test_output");

        for file in fs::read_dir(&filetest_dir).unwrap() {
            let file = file.unwrap();
            let path = file.path();

            if path.extension() == Some(OsStr::new("input")) {
                filetest_count += 1;

                let mut sequencer = Sequencer::default();

                sequencer.update(Command::StartWrite).unwrap();

                let input = fs::read_to_string(&path).unwrap();

                for cmd in input.lines() {
                    sequencer.update(ron::from_str(cmd).unwrap()).unwrap()
                }

                let mut midi_name = PathBuf::from(path.file_name().unwrap());

                midi_name.set_extension("mid");

                sequencer
                    .update(Command::EndWrite(output_dir.join(&midi_name)))
                    .unwrap();

                let mut expected = vec![];
                let mut output = vec![];

                File::open(filetest_dir.join(&midi_name))
                    .unwrap()
                    .read_to_end(&mut expected)
                    .unwrap();
                File::open(output_dir.join(&midi_name))
                    .unwrap()
                    .read_to_end(&mut output)
                    .unwrap();

                assert_eq!(expected, output);
            }
        }

        assert_eq!(filetest_count, 1);
    }
}
