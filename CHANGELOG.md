## (unreleased)

### Monument

- (#17, #24) Add a guide for the TOML input format (#24 fixes a mistake in the example)
- (#21) Allow course-head masks to be specified per-method
- (#24) Add `examples/` and `to-complib.py` to the pre-built releases.
- (#25) Fix off-by-one error when outputting part heads of multi-part compositions

### BellFrame

- (#25) Add `Row::order`, along with `RowAccumulator::accumulate_unchecked` and
    `RowAccumulator::pre_accumulate_unchecked`.
