## (unreleased)

### Monument

- (#17, #24) Add a guide for the TOML input format (#24 fixes a mistake in the example)
- (#21) Allow course-head masks to be specified per-method
- (#24) Add `examples/` and `to-complib.py` to the pre-built releases.
- (#25) Fix off-by-one error when outputting part heads of multi-part compositions
- (#36) Remove self-false nodes even in single-parts.  This happens if a method is false within its
    own lead.  I can't see why anyone would ask Monument for such a thing, but if you do then
    Monument will now correctly declare it impossible.
- (#37) Add extra integration tests for false method splices (i.e. a splice between mutually-false
    leads) and half-lead calls.
- (#38) Rename `Node` to `Chunk` and `-D search` to `-D no-search` for clarity

### BellFrame

- (#25) Add `Row::order`, along with `RowAccumulator::accumulate_unchecked` and
    `RowAccumulator::pre_accumulate_unchecked`.
- (#36) Add `Truth`, as a `bool`-like for representing truth in a way that the compiler checks.
