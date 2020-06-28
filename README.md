# Simple MIDI sequencer

A simplistic sequencer that acts on MIDI files. It supports "playing" the
sequence (actually just printing the sequence and parameter changes to
stdout) or writing to a MIDI file. You can choose from multiple voices,
with different sets of parameters, which are encoded using MIDI program
changes and MIDI CC for the parameters.

The sequences can be arbitrary fractions of the host BPM, although if the
divisor isn't a multiple of 3 or 2 then it will probably slowly drift
because of inaccuracies in the division of `TICKS_PER_BEAT`. A real project
would prevent this at runtime by disallowing values for this BPM fraction
that would cause inaccuracies, or possibly by using floating-/fixed-point
maths along with some extra logic to keep the tracks in sync.

## Usage

The input is just an enum with a `serde::Deserialize` impl, deserialized
with [RON](https://github.com/ron-rs/ron). So for a list of the available
commands, see the `Command` enum, and for how to write that on the command 
line, see the documentation for RON. For an example of how to set parameters
and otherwise manipulate the sequencer, see `filetests/simple.input`. When
you want to see the output, either use `Start(..)` to write the messages
to stdout for the given period of time, or use `StartWrite`, use some
commands (such as `Tick` or even `Start`) and then save an output MIDI
file using `EndWrite("/path/to/output")`. The syntax of RON is very
similar to that of Rust, so it should be pretty simple to understand.

### Some notes

- The code is a little overengineered, as I had committed to being able to
  save MIDI files, to being able to edit the sequences on-the-fly, and to
  being able to step the sequencer by arbitrary numbers of ticks. These
  constraints are self-imposed, though, and in a real project you might be
  able to relax some of them in response to the needs of the project.
- Relatedly, I've kept all the code in one file since (in my opinion) it makes
  it easier to review the whole project on GitHub and similar. It's still below
  1000SLC (much of the size of the file is comments and docs) so it's debatable
  whether or not it should be split into modules yet.
- I've chosen not to make it multi-threaded, so while you can edit sequences
  while they're playing, this is done by using a `Tick` function to progress
  time. In other words, it's asynchronous but not concurrent. I did this so
  that when operating it on the command line you don't have the input and output
  overwriting one another. The best way to make this multi-threaded would be to
  separate the parts of `Sequencer` and `Sequence` which are updated in
  response to user input but only read on tick, such as the sequences
  themselves, from the parts of `Sequencer` and `Sequence` which are unneeded
  when handling user input but written on tick, such as `Sequence::cur_note`.
  You could also just put the whole `Sequencer` in a `Mutex`, which is a lot
  easier and should do the job in most cases.
- In many places I've returned an `impl Iterator` instead of a collection,
  even when the iterator is just `arrayvec::IntoIter`. `impl Iterator` is
  the only form of `impl Trait` that I use by default, because with collections
  I've found it's very common to start by eagerly generating a collection for
  simplicity, and then go back later to make it a lazy iterator with `O(1)`
  space complexity.
- While it was mostly built with an embedded environment in mind, we need
  to allocate in order to save MIDI files (this isn't inherent in saving
  MIDI files, just an artifact of the library chosen). Additionally, we
  use floating-point maths in order to deal with seconds, but this is just
  for convenience and we could use fixed-point maths as long as we're happy
  to choose a resolution that makes sense for the project. I've also avoided
  `unwrap` except for the results of constant expressions and in `main`.
- I use MIDI as my interchange format, both internally and for writing to
  a file. In a real project we might find that MIDI's restrictions mean that
  using it internally is more trouble than it's worth, especially the 
  restriction of notes to semitones and CCs to 8 bit values.
- Relatedly, I use `ghakuf` as my MIDI library, basically just because it's
  the first library that I found that supports writing MIDI files. I wouldn't
  use this in a real project because it requires allocation and doesn't
  support streaming serialisation of MIDI files. In fact, its method of
  writing MIDI files is really weird, where it has an internal
  `Vec<&'a Message>` instead of `Vec<Message>`, forcing you to have two
  buffers, one for the owned data, and one inside `ghakuf` of references to
  that owned data.
