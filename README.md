# BellFrame

BellFrame is a library of human friendly data types for concepts which are ubiquitous in change
ringing (e.g. `Bell`, `Row`, `Stage`, `Method`, etc.).  The aim is to provide a simple library for
with fast and correct implementations of common operations, rather than providing utilities for
every use case.

All data types are written so that undefined behaviour is impossible without using `unsafe`.
However, `unsafe` versions of most functions are provided for cases where the required invariants
are upheld but can't be verified by the compiler.

I am currently using this in several Rust projects, and the API is very nascent and may change
radically between minor version numbers until 1.0 (e.g. `0.1.0` to `0.2.0` will likely be a breaking
change).

## Goals

The goals of BellFrame are, in order of importance:

- Reliability: If the user never uses `unsafe` code, then undefined behaviour of any kind should not
  be possible.  All errors should be handled nicely even for pathological inputs.
- Generality: BellFrame should be able to handle anything the user might want to throw at it, within
  the scope of the library (e.g. there is no upper stage limit, or assumptions about method
  conventions).
- Performance: BellFrame should not be unnecessarily slow or use unnecessary memory.  Performance
  optimisations are fair game, provided they don't conflict with higher goals or explode the code
  complexity (making reliability harder to maintain).
