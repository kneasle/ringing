# Ringing

A single repository containing all my Rust projects related to
[change ringing](https://en.wikipedia.org/wiki/Change_ringing).  All are under the MIT license.

As far as cargo/rust is concerened, this repository is one single workspace consisting of many
separate crates.  Each project/library lives in its own directory and may correspond to multiple
Rust crates.  Having all of these in the same repository ensures that all code is kept synchronised,
and CI builds don't have to worry about dependency shenanigans.

### Projects

- [`Monument`](monument/): A fast and flexible library & CLI app for generating compositions
- [`Jigsaw`](https://github.com/kneasle/jigsaw) (yet to be moved here): A visual tool for
  experimenting with compositions

### Libraries
- [`BellFrame`](bellframe/): A 'standard library' containing robust primitives useful for processing
  compositions in Rust.  Used by all other projects.
- [`utils`](utils/): Small utilities which aren't specific to ringing.  Mostly extensions to the
  standard library, with few or no dependencies.
