## (Unreleased)

### Monument
- (#82) Add `calling_bell` parameter to override the bell used when determining calling positions
    (a.k.a. the 'observation' bell).  If unspecified, this defaults to the heaviest bell in the
    stage.
- (#81) Fall back on a default music profile if no music is specified.  `default_music = false` will
    disable this.
- (#80) Add error for CHs that aren't in other parts.
- (#79) Generate a progress update as the search finishes.
- (#79) Print number of compositions generated in the progress line.
- (#77) Tie-break equally musical compositions by their average overall score.
- (#73) `splice_style = "calls"` will no longer generate splices over part heads.
- (#73) `splice_weight` is now applied to splices over part heads.
- (#73) `splice_style = "calls"` now works for cyclic compositions.
- (#71) Allow specifying an exact count with e.g. `count = 224` rather than
    `count = { min = 224, max = 224 }`.

---



## 28th March 2021

### Monument v0.7.0
- (#70) New graph optimisation: remove links between mutually false chunks.
- (#69) Allow weights on method splicing with `splice_weight` (defaults to 0).
- (#67) Add error messages for giving the same debug/display name to multiple calls (specifically,
    two calls at the same lead location but with different place notations).
- (#66) Allow method counts to be overridden per-method (by adding a `count` parameter to methods).

### Internal Improvements
- (#64) Use `goldilocks-json-fmt` to format the test result files.

---



## 10th March 2021

### Monument v0.6.0

#### Headline
- (#58) Add nice error messages for all custom errors (TOML parsing errors are still lacking, but
    they are substantially harder to fix).
- (#54) Print comp list even when a search is aborted with `ctrl-C`.
- (#50) Add `bobs_only` and `singles_only`.

#### Smaller Fixes
- (#58) Allow `calling_positions = "<string>"` to set the calling positions of a call to the
    characters in `<string>` (previously `calling_positions` had to be an array).
- (#56) Allow `to-complib.py` to handle multiple-letter method shorthands.
- (#55) Print warning for using plain-bob style calls in Grandsire or Stedman (Grandsire and Stedman
    are still pretty buggy anyway, though).
- (#54) Don't bother freeing memory allocated during the search algorithm.  This makes Monument
    terminate instantly, often shaving 10s of seconds from the search time.
- (#53) Add limit on graph size.  Set with `--graph-size-limit`, defaults to 100K chunks.

#### Internal
- (#58) Allow multiple test cases to be stored in one file
- (#57) Fix mistake in `guide.md` which turned a large part of the guide into a giant code block.
- (#50) Fix some dead links in Monument's guide.

### BellFrame v0.8.1
- (#58) Fix incorrect indices for `PnBlockParseError`

---



## 18th Feb 2021

### Monument v0.5.0
- (#48) Allow `start_indices`/`end_indices` to be overridden for each method.
- (#48) Allow negative values for `start_indices`/`end_indices` (still relative to 0 as a standard
    start).
- (#49) Sort final composition list by absolute music score.

### BellFrame v0.8.0
- (#47) Rename `AnnotBlock` to `Block` (removing the type-def `Block = AnnotBlock<()>`).
- (#47) Use `u8` instead of `usize` as the underlying representation for `Bell`, `Stage` and places.

### Internal Improvements (Monument)
- (#48) Make integration tests error on unspecified/new test cases.
- (#44) Rewrite the test harness, which now doesn't depend on Python and also allows error messages
    to be tested.
- (#43) Add READMEs on Monument's pages [on crates.io](https://crates.io/crates/monument).

---



## 12th Feb 2021

### Monument v0.4.0
- (#42) Add `handbell_coursing_weight` to easily add `ch_weights` for handbells in their coursing
    positions.
- (#41) Display score from music in CLI output, rather than the total scores.
- (#40) Set default of `num_comps` to 100.

---



## 1st Feb 2021

### Monument v0.3.0
- (#39) Rework all `README`s and Monument's guide to make them more clear and readable.
- (#36) Remove self-false nodes even in single-parts.  This happens if a method is false within its
    own lead.  I can't see why anyone would ask Monument for such a thing, but if you do then
    Monument will now correctly declare it impossible.
- (#31) Output large numbers in a human-friendly way (e.g. `10.3M` rather than `10300000`).  Similar
    pretty output for the search time (e.g. `5m 32.3s` rather than `332.3s`)

### BellFrame v0.7.0
- (#36) Add `Truth`, as a `bool`-like for representing truth in a way that the compiler checks.

### Internal Improvements
- (#38) Rename `Node` to `Chunk` and `-D search` to `-D no-search` for clarity
- (#37) Add extra integration tests for false method splices (i.e. a splice between mutually-false
    leads) and half-lead calls.
- (#32) Run CI in debug mode for a small speed gain.

---



## 18th Jan 2021

### Monument v0.2.3
- (#26) Fix off-by-one error when outputting part heads of multi-part compositions
- (#17, #24) Add a guide for the TOML input format (#24 fixes a mistake in the example)
- (#21) Allow course-head masks to be specified per-method
- (#24) Add `examples/` and `to-complib.py` to the pre-built releases.

### BellFrame v0.6.0
- (#26) Add `Row::order`, along with `RowAccumulator::accumulate_unchecked` and
    `RowAccumulator::pre_accumulate_unchecked`.
