# Per-Method Parameters

These parameters only affect a specific method and so are specified within the top-level `method`
or `methods` parameters.

Some of these (like `count`, `{start,end}_indices` or `courses`) override top-level parameters on
a per-method basis.  Others (like `shorthand` or `labels`) can only be applied per-method and have
no top-level counterparts.

**Contents:**
- [`title`](#by-title) or [(`name`, `place_notation` and `stage`)](#by-place-notation)
- [`shorthand`](#shorthand)
- [`labels`](#labels)
- Overrides for global properties
  - [`count`](#count) (overrides `method_count`)
  - [`courses`](#courses)
  - [`start_indices`](#start_indices)
  - [`end_indices`](#end_indices)

---

**TODO:** Docs for each parameter
