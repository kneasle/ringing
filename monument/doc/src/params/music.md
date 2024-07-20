# `music`

**Defaults to `[]` (i.e. no custom music).**

Array of custom music types:

```toml
[[music]]
run_lengths = [4, 5, 6, 7, 8] # or a single length: `run_length = 4`
internal = true               # Optional; defaults to `false`
# or
patterns = ["*6578", "6578*"]       # or a single pattern: `pattern = "*5x6x7x8*"`
count_each = { min = 12, max = 24 } # Count range applied per-pattern.
                                    # Optional; defaults to allowing anything

# common values:
weight = 2    # Score applied per instance of this music type.  Optional; defaults to `1`
count = { min = 12, max = 24 } # Overall required count range
stroke = "back" # On which stroke(s) to count this music.
                # Options: "both" (default), "back", "hand".
show = true # If `true`, display this music in the composition summary.
            # Optional; defaults to `true`
name = "87s at back" # If `show = true`, sets a custom name used in the summary output.
                     # By default, Monument will decide how to display music (often combining
                     # separate patterns together)
```
