# Starts/Ends

Parameters which describe how the composition starts and ends.

**Contents:**

- [`snap_start`](#snap_start)
- [`start_indices` and `end_indices`](#start_indices-and-end_indices)
- [`start_stroke`](#start_stroke)

---

## `snap_start`

If no `start_indices` have been set, `snap_start = true` allows just snap starts (i.e. is equivalent
to `start_indices = [2]`).  Defaults to `false` (i.e. just lead end starts).

## `start_indices` and `end_indices`

Sets the indices within the lead where the composition can start/end.  The default value of
`start_indices` is determined by `snap_start`, whereas the `end_indices` defaults to allowing any
value. These indices are taken modulo the lead length and can be negative, so for example 2, -30 and
34 would all refer to the backstroke snap in treble dodging Major.

## `start_row` and `end_row`

**_(since v0.10.0)_**

Specifies the row used to start or finish the composition, both defaulting to rounds.  These don't
work with multi-parts, but are useful for things like getting Monument to extend 720s of Minor into
a quarter-peal length.

## `start_stroke`

The stroke of the first non-rounds row (technically, the first row that isn't `start_row`).
```toml
start_stroke = "hand" # Default
# or
start_stroke = "back"
```
Before v0.10.0, this was reversed (i.e. the opposite way round to what people expect).
