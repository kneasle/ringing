# Ringing Monorepo

A single workspace containing all my Rust projects related to
[change ringing](https://en.wikipedia.org/wiki/Change_ringing).  All are under the MIT license.

Each project/library lives in its own directory and may correspond to multiple Rust crates:
- [`Monument`](monument/): A fast and flexible library & CLI app for generating compositions
- [`Jigsaw`](https://github.com/kneasle/jigsaw) (yet to be moved here): A visual tool for
  experimenting with compositions
- [`BellFrame`](bellframe/): A 'standard library' containing robust primitives useful for processing
  compositions in Rust.  Used by all other projects.

Having all of these in the same repository ensures that all code is kept synchronised, and CI builds
don't have to worry about dependency shenanigans.
