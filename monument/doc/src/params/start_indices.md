# `start_indices`

**Defaults to `[0]` if [`snap_start = false`](snap_start.md) (the default), or `[2]` if
[`snap_start = true`](snap_start.md).**

Sets the indices within each method's lead where the composition can start.  These indices are
taken modulo the lead length and can be negative, so for example 2, -30 and 34
would all refer to the backstroke snap in treble dodging Major.

This can be overridden per-method using [the corresponding `start_indices`
parameter](method/start_indices.md).

See also [`end_indices`](end_indices.md).
