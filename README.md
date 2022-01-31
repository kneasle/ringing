# Ringing

A single repository containing all my Rust projects related to change ringing.  More info about
change ringing can be found [here](https://en.wikipedia.org/wiki/Change_ringing), though if you're
not into ringing this whole repo probably won't make much sense...

### Projects

- [`Monument`](monument/): A fast, flexible and human-friendly composing engine
- [`Jigsaw`](https://github.com/kneasle/jigsaw) **(yet to be moved here)**: A visual tool for
  experimenting with compositions

### Libraries

- [`BellFrame`](bellframe/): A 'standard library' for change ringing, shared between all projects.
- [`utils`](utils/): Small utilities which aren't specific to ringing.  Mostly extensions to the
  standard library, with few or no dependencies.  Also shared between projects.

---

As far as cargo/rust is concerned, this repository is one single workspace consisting of many
separate crates.  Each project/library lives in its own directory and may correspond to multiple
Rust crates.  Having all of these in the same repository ensures that all code is kept synchronised,
and CI doesn't have to worry about dependency shenanigans.

All projects are under the MIT license.
