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
