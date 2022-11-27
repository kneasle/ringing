# Defining Music

These parameters describe what music Monument should care about.

**Contents:**

- [`base_music`](#base_music)
- [`music_file`](#music_file)
- [`music`](#music) array:
  - [`weight`](#weight)
  - [`count`](#count)
  - [`stroke`](#stroke)
  - [`show` and `name`](#show-and-name)

---

## `base_music`

Like [`base_calls`](#base_calls), `base_music` adds a set of basic music definitions to the search.
`base_music` has two values, the default being `base_music = "default"`:

1. `base_music = "none"`:  Adds no music to the search; the only music will be what you explicitly
   specify.
2. `base_music = "default"`:  For most stages, this adds a basic music profile intended to be a sane
   default for most music tastes.  This roughly follows the 'headline' music in CompLib (i.e. the
   summary line shown below every composition.

   The default music profiles are equivalent to importing the following music files:
   - [Minor](src/default-music-minor.toml)
   - [Triples](src/default-music-triples.toml)
   - [Major](src/default-music-major.toml)
   - [Royal](src/default-music-royal.toml)
   - [Maximus](src/default-music-maximus.toml)

## `music_file`

Relative path to a file containing music definitions (i.e. a single [`music` array](#music-array)).

This provides an easy way to share music between multiple composition files.

## `music`

**TODO:** Write entries for each parameter

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
