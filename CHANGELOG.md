## (Unreleased)

### Monument v0.15.0

---



## 2nd September 2023

### Monument v0.14.0

#### Headline Features
- (#284) Allow scoring of all-the-work compositions, using the `atw_weight` parameter.
- (#306) Add new option `base_music = "complib"`, which adds base music which exactly matches that
    of CompLib (except that Monument can't count wraps).  Thanks, @samuelsenior!

#### Smaller Changes
- (#205) Remove chunks and links which contribute too much method counts.  This should have a small
    speed improvement for compositions which include one lead of each method in each part
    (e.g. 23-spliced Surprise Major).
- (#221) In the guide, always use valid values for `{start,end}_rows`.
- (#309) Align columns correctly when the maximum composition length is 2-digits long.

#### Internal Improvements
- (#203) Automate the release workflow.  Now, `cargo cut-release` is enough to trigger the
    whole release pipeline.
- (#204, #304, #310) Implement benchmark runner, and add benchmarks from real use-case (first
    contribution; thanks @samuelsenior!).
- (#296) Add `ARCHITECTURE.md` to help new contributors getting an idea of the code
- (#303) Completely rewrite the interface of Monument's internal library to make it more amenable
    to a graphical interface.

### Bellframe v0.12.0
- (#221) Add `Bell::MAX`, which returns the largest `Bell` possible (i.e. the 254th)
- (#221) Stop bell-path methods (e.g. `Block::path_of`) from returning `Option`.  Instead, passing
    a `Bell` out of the `Block`'s `Stage` will cause a panic.
- (#221) Add `Mask::contains`, returning `true` if the `Mask` constrains that `Bell`.

---



## 1st January 2023

### Monument v0.13.4

#### Headline Features
- (#201) Print an extra header line every 50 compositions.
- (#200) Remove `debug_symbol` and calculate it automatically for bobs.
- (#200) Replace `allow_false` with `require_truth`
- (#185) Rename `ch_weights` to `course_weights`, and `course_heads` to `courses`

#### Internal Improvements
- (#167) Remove dependence on `serde` from Monument's library.  Now, all the handling of TOML files
    is in `monument_cli`.
- (#171) Don't store test cases in Markdown files, instead storing them all as individual TOML
    files.  Apart from massively simplifying the code, this also means that all test cases can be
    easily run directly from the command line (a very useful feature for debugging).
- (#172) Test Monument's `stdout` output, rather than driving the library directly.
- (no PR) Fix test case path that was invalid on Windows.

---



## 21st October 2022

### Monument v0.12.0

#### Headline Features
- (#156) Implement non-duffer pruning.  In short, you specify courses which are 'non-duffer' (e.g.
    those with 4-bell runs) and then you can enforce a limit on how much contiguous/total 'duffer'
    rows can be rung. Think MBD's no-duffer Bristol, but this works for any composition.
- (#143) Use explicit memory limit.  Set by `--mem-limit` or `-M`, defaults to ~~90%~~ 80% of
    available system memory.

#### Internal Improvements
- (#165) Only require clippy lints on releases.
- (#146) Merge fields of `graph::build::MethodData` into `query::Method`.
- (#142) Implement builder API to make `monument` easier to use as a library
- (#142) Make `Query` private and build `Search`es directly.
- (#140) Rename some fields of `Query` (which, after #138, are no longer exposed externally)
- (#138) Heavily clean up the API of the `monument` library.  This PR focusses on removing as much
    API surface as possible, so that the remaining API can be made as easy as possible to use.  With
    some more attention, it should be possible to embed the `monument` library into programs other
    than its CLI interface.

#### Pre-Release Bug Fixes
- (#166) Output correct contiguous duffer lengths in multi-parts.
- (#166) Make `Chunk`s non-duffer only if they are a non-duffer in **every** part.
- (#145) Reduce memory limit from 90% to 80% of the available memory.

### BellFrame v0.11.0
- (#140) Rename `Mask::fix_bells` to `Mask::with_fixed_bells`.
- (#140) Don't return borrowed data in the error from `MethodLib::get_by_title_with_suggestions`.
- (#156) Only implement `Add`/`Sub` for `Bell` with `i16` (i.e. not `u8` or `i8`).  If you want to
    add/subtract `Bell`s with `i8` or `u8`, cast them to `i16`.
- (#156) Add `Method::lead_end` (to complement `Method::lead_head`).
- (#156) Add `Mask::from_{bells,vec}` and `Mask::bells`.

---



## 31st July 2022

### Monument v0.11.0

#### Headline Features
- (#116) Much smarter way of determining default method balance.  (Nerdy details:) Method counts are
    weighted by the square root of each method's lead length (so shorter methods won't need as many
    rows), and will round 'outwards' for cases like cyclic spliced where a 'perfect' method balance
    actually has a wide range of different counts.
- (#121) Renamed `lead_location` to `label` (for brevity).
- (#120) Allow `queue_limit` and `graph_size_limit` to be set in the TOML format.
- (#116) Prove which lengths and method counts are actually possible, and error if the two can't
    match.

#### Smaller Changes
- (#122) Stop graph optimisation when all passes fail to make progress once (rather than waiting for
    an entire run of passes to not make progress).
- (#115) Demote 'default music' message from `WARN` to `INFO`.
- (#112) Massively speed up falseness generation when using custom CHs in cyclic comps, by computing
    all the false lead heads rather than computing falseness between all pairs of masks.

#### Bug Fixes
- (#124) Fix incorrect output for single-method cyclic comps.

#### Internal Improvements
- (#113) Remove unnecessary threading from the search code (which can currently only use one thread
    anyway).
- (#112) Refactor `FalsenessTable::new` into multiple helper functions
- (#110) Exclusively use `{Total,PerPart}Length` to refer to lengths (as opposed to `usize`), thus
    allowing the compiler to spot when we mix them up.
- (#109) Encapsulate all the part-head logic into `PartHeadGroup`/`PartHead`/`PhRotation`.

### BellFrame v0.10.0
- (#117) Rationalise the use of `unsafe` in `bellframe`.
- (#115) Remove unnecessary `unsafe` in `bellframe::music`.
- (#115) Fix integer underflow when computing internal runs.
- (#115) Implement `Ord` for `Mask`
- (#112) Rename `Mask::combine` to `Mask::intersect`

---



## 4th July 2022

### Monument v0.10.0

#### Headline Features
- (#105) Completely rewrite and simplify the graph building code.  Lots of things relating to
    `course_heads` and multi-parts should now Just Work™:
    - Incompatible course heads are no longer a thing.  You can now do things like
        `course_heads = ["*78", "12345*"]` and it will Just Work™.  Previously this would error
        because `12345867` could be given two different course heads.
    - `course_heads` always Just Works™, even in e.g. cyclic multi-parts.  Previously cyclic comps
        would simply ignore `course_heads`.
    - Specifying multiple start/end indices now Just Works™ in multi-parts - i.e. snap
        start/finishes are allowed, but Monument won't mix them or put an illegal splice over the
        part head.  This would previously cause a crash (found by Jadd Virji - thanks!).
- (#96) Add music presets for:
    - Near misses (for any stage)
    - CRUs (for >= Triples)
    - 5678 combinations (for both Triples and Major)
    Load them with, e.g.:
    ```toml
    music = [
        { preset = "5678 combinations" },
        { preset = "near misses" },
        { preset = "crus" },
    ]
    ```

#### Smaller Features
- (#105) Add `start_row` and `end_row` for making compositions start/stop at a row other than
    rounds.  Useful for using Monument to extend 720s to get QPs of Minor.
- (#105) Remove `splice_style = "call locations"` (which would allow splices only where a call
    _could_ have been made).
- (#105) `start_stroke` now refers to the row _after_ `start_row` (i.e. the first non-rounds row).
- (#104) Fix column alignments for (a) negative scores and (b) long (i.e. at least 5-digit) lengths.

#### Bug Fixes
- (#105) Fix bug where Monument would, in obscure situations, produce false compositions (found by
    David Thomas - thanks!).  Monument now expands the rows of each composition generated and
    explicitly checks for truth - so if falseness bugs do creep in, you'll know about it (and
    hopefully the large test suite will catch it before it reaches you).

#### Internal Improvements
- (#105) Remove `monument::Layout` and add `Method`s and `Call`s explicitly to a `Query`.
- (#104) Make all test cases deterministic by (a) rounding the composition scores and (b) making
    sure that all test cases are exhaustive searches (to negate Monument's non-deterministic search
    order).

### BellFrame v0.9.0
- (#104) Implement `Ord` for `Stroke`.
- (#104) Add `Mul` implementations for every combination of `&Row`/`&RowBuf`/`RowBuf` versus
    anything from `&Row`/`&RowBuf`/`RowBuf` or `&Mask`/`Mask`.
- (#104) Fix bug in `Block::extend_range`, where too many annotations would be copied.
- (#104) Add new methods:
    - `Row::copy_from`: in-place write to an `&mut Row` (i.e. requiring the stages to match),
        analogous to `<[T]>::copy_from_slice`.
    - `Block::with_leftover_row`: create a new `Block` with only the specified leftover row.
    - `Block::leftover_row_mut`: to mutably borrow the leftover row of a `Block`.
- (#97) Enforce extra invariants for `music::Pattern` (making it much more robust, at the cost of
    needing to handle some errors that should have been handled anyway).
- (#97) Rename `music::Regex` to `music::Pattern` (it isn't anywhere near as powerful as true
    regexes).
- (#96) Add `Stage::extent`, which returns a `SameStageVec` containing every possible `Row` of that
    `Stage` _in an arbitrary order_.
- (#96) Allow addition/subtraction between `Stage`s and `u8`s with `+`/`-`, panicking on overflow or
    a `Stage` of 0.  `checked_add` and `checked_sub` are the non-panicking versions.
- (#96) Add conversions from `Row`/`RowBuf` to `Mask` and `Regex` (via the `From` trait)

---



## 25th May 2022

### Monument v0.9.0

#### Headline Features
- (#94) Replace `default_music` with `base_music` (to be consistent with `base_calls`).
- (#92) Print music as part of the composition summary.

#### Smaller Features
- (#95) Suggest using `{bob,single}s_only = true` if `{single,bob}_weight` is set to a large
    negative value.  `{bob,single}s_only` is faster than using `{single,bob}_weight`, but sometimes
    both call types are _required_ to bring a composition round with the right length so Monument
    can't automatically set `{bob,single}s_only`.
- (#92) Remove fixed tenors from part heads in summary (e.g. `1342567890ET` is now be just `1342`).
- (#91) Calls can now go from/to different lead labels.  Set this with e.g.
    `label = { from = "2nds", to = "HL" }`.  Useful for adding finer control over where
    calls can be placed.
- (#91) Allow multiple labels on the same row within a lead.  Also reversed the syntax from e.g.
    `labels = { 0 = "LE", 16 = "HL" }` to `labels = { LE = 0, HL = 16 }`.  The same label can be
    added to multiple rows like `labels = { SE = [3, 9] }` (for Six-Ends in Stedman).

#### Internal Improvements
- (#89) Refactor the search algorithm (splitting the node expansion from the best-first search code)

### BellFrame v0.8.5
- (#92) Give `Regex`es a specific `Stage`.
- (#91) Allow multiple lead labels to be placed on the same row

---



## 2nd May 2022

### Monument v0.8.1

- (No PR) Fix incorrect 'ETs at back' to 'TEs at back' in the default music

### Monument v0.8.0 (yanked)

#### Headline Features
- (#82) Add `calling_bell` parameter to override the bell used when determining calling positions
    (a.k.a. the 'observation' bell).  If unspecified, this defaults to the heaviest bell in the
    stage.
- (#81) Fall back on a default music profile if no music is specified.  `default_music = false` will
    disable this.  Now, Monument will produce good results even if you only specify `length` and
    `method`.

#### Bug Fixes
- (#73) `splice_style = "calls"` will no longer generate splices over part heads.
- (#73) `splice_weight` is now applied to splices over part heads.
- (#73) `splice_style = "calls"` now works for cyclic compositions.

#### Smaller Features
- (#80) Add error for CHs that aren't in other parts (e.g. setting 
    `course_heads = ["*78", "*7856"]` and `part_head = "134265"` now produces an error, because
    `*7856` becomes `*7865` in even-numbered parts).
- (#79) Generate a final 'search complete' progress update just after the search finishes.
- (#79) Print number of compositions generated in the progress line.
- (#77) When outputting compositions, tie-break equally musical compositions by their average
    overall score per row.
- (#71) Allow specifying an exact count with e.g. `count = 224` rather than
    `count = { min = 224, max = 224 }`.

---



## 28th March 2022

### Monument v0.7.0
- (#70) New graph optimisation: remove links between mutually false chunks.
- (#69) Allow weights on method splicing with `splice_weight` (defaults to 0).
- (#67) Add error messages for giving the same debug/display name to multiple calls (specifically,
    two calls at the same lead location but with different place notations).
- (#66) Allow method counts to be overridden per-method (by adding a `count` parameter to methods).

### Internal Improvements
- (#64) Use `goldilocks-json-fmt` to format the test result files.

---



## 10th March 2022

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



## 18th Feb 2022

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



## 12th Feb 2022

### Monument v0.4.0
- (#42) Add `handbell_coursing_weight` to easily add `ch_weights` for handbells in their coursing
    positions.
- (#41) Display score from music in CLI output, rather than the total scores.
- (#40) Set default of `num_comps` to 100.

---



## 1st Feb 2022

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



## 18th Jan 2022

### Monument v0.2.3
- (#26) Fix off-by-one error when outputting part heads of multi-part compositions
- (#17, #24) Add a guide for the TOML input format (#24 fixes a mistake in the example)
- (#21) Allow course-head masks to be specified per-method
- (#24) Add `examples/` and `to-complib.py` to the pre-built releases.

### BellFrame v0.6.0
- (#26) Add `Row::order`, along with `RowAccumulator::accumulate_unchecked` and
    `RowAccumulator::pre_accumulate_unchecked`.
