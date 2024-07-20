# `snap_start`

**Defaults to `false` (i.e. just lead end starts).**

Changes the default behaviour of [`start_indices`](start_indices.md) to allow only snap starts.

Monument determines the possible composition starts using the following flow:

- If [`start_indices`](start_indices.md) has been set, then use those.
- Otherwise:
  - If `snap_start = true`, allow only snap starts (equivalent to `start_indices = [2]`).
  - If `snap_start = false`, allow only lead-end starts (equivalent to `start_indices = [0]`).
