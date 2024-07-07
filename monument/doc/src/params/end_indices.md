# `end_indices`

**Defaults to allowing ending at any index within a lead.**

Sets the indices within a lead where the composition can end.  These indices are taken modulo each
method's lead length and can be negative, so for example 2, -30 and 34 would all refer to the
backstroke snap in Treble Dodging Major.

This can be overridden per-method using [the corresponding `end_indices`
parameter](method/end_indices.md).

See also [`start_indices`](start_indices.md).
