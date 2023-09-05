# Monument CLI Guide

**NOTE:** There is no guarantee of the stability of this format; it is, in theory, able to change at
any point without warning.  I'm working on a GUI for Monument which, unlike that of Wheatley in
Ringing Room, will be fully featured and will become the intended way to use Monument (the core Rust
library is still accessible to anyone who wants to do really complex things).  I won't make breaking
changes to the TOML format for the fun of it, but it may change to reflect the GUI layout.  The GUI
also won't obsolete the TOML-based format (I use it for testing purposes), but it's only really
intended for my own use and I won't recommend it for users once the GUI exists.

## Contents

- [Installation](#installation)
- [Example](#example)
- [Quick List of Parameters](#quick-list-of-parameters)
- [More Detail on Parameters](#more-detail-on-parameters)

---

## Installation

If you already have Rust installed, you can get the latest version of Monument with `cargo install
monument_cli`.  If you don't already have Rust ~what are you doing with your life~ you can download
a pre-built copy of the latest version from
[here](https://github.com/kneasle/ringing/releases/latest).

### Library `libssl.so.1.1` not found

If you're installing on some Ubuntu systems, you may get an error when running Monument that the
library `libssl.so.1.1` can't be found.  libSSL is a system library for handling web connections,
which Monument uses to download the Central Council's method library.

It can be installed on Ubuntu by running the following commands at a terminal:
```bash
wget http://nz2.archive.ubuntu.com/ubuntu/pool/main/o/openssl/libssl1.1_1.1.1f-1ubuntu2.19_amd64.deb
sudo dpkg -i libssl1.1_1.1.1f-1ubuntu2.19_amd64.deb
```

---

## Example

More examples can be found in [the `examples/` directory](examples/).  This exact example can also
be found there (as [`examples/guide.toml`](example/guide.toml) and
[`examples/guide-music-8.toml`](examples/guide-music-8.toml)).  Monument's CLI reads files are in
the TOML data format, so if you're wondering then check out TOML's
[helpful website](https://toml.io/en/) for more info.

The following example is for quarter peal compositions of Yorkshire, Lessness and Bastow with
changes of method only at calls, and a 2-part with `124365` as the part-head.  It's designed to
cover as many areas of Monument's interface as possible, hence it's rather complex.

### File: `guide.toml`
```toml
# General
length = "QP"
num_comps = 10 # just for brevity; you probably want more comps than this

# Methods
methods = [
    "Yorkshire Surprise Major",
    { title = "Lessness Surprise Major", shorthand = "E" },
    { name = "Bastow", place_notation = "x2,1", stage = 8 },
]
splice_style = "calls"
method_count = { min = 100, max = 600 } # relax method balance to allow for Bastow

# Calls
base_calls = "near" # optional, since this is the default
bob_weight = -7
single_weight = -10
calls = [{ symbol = "b", place_notation = "1456", weight = -12 }]

# Music
music_file = "guide-music-8.toml" # Relative to this file, so expects `music-8.toml` in the same folder
music = [
    { patterns = ["5678*", "8765*"], weight = 2 }, # Boost music off the front
    { pattern = "*87", weight = -1, stroke = "back" }, # Slightly penalise 87s at back
]

# Courses
part_head = "124365"
split_tenors = true
# courses = ["*78", "*7856"] # uncomment to override `split_tenors`
leadwise = false # optional: Monument would have determined this
course_weights = [{ pattern = "*78", weight = 0.05 }] # slight boost for tenors-together courses

# Starts/Ends (commented because they currently play badly with multi-part spliced)
# snap_start = true
# end_indices = [0] # only allow non-snap finishes
```

### File: `guide-music-8.toml`

```toml
# This file contains just an array called `music`, and can be imported into other files.  This is
# useful for e.g. default music schemes
[[music]]
run_lengths = [4, 5, 6, 7, 8]
internal = false # optional; this is the default

[[music]]
pattern = "*6578"
weight = 1.2

[[music]]
patterns = ["*5678", "*8765", "5678*", "8765*"]
weight = 0.5
```

### Output

Running `monument_cli guide.toml` outputs:

```text
-- snip --

SEARCH COMPLETE!

len: 1264, ms: [576, 576, 112], score: 137.40, avg: 0.108703, str: BBBB[bB]EEE[H]EEE[B]EEE[sH]YYYY[W]YYYYY[B]BBBBBBB[B]BBB
len: 1296, ms: [576, 512, 208], score: 141.80, avg: 0.109414, str: BBBB[B]YYYY[B]YYYYY[M]BBBBBB[W]E[sH]BBBB[B]E[V]BBBBBBB[F]E[bB]EEE[H]BBBB[B]EE[W]B
len: 1272, ms: [576, 512, 184], score: 139.30, avg: 0.109513, str: BBBB[B]EEE[bH]YYYY[W]YYYYY[B]BBBBBBB[bB]BBBBBBB[B]EEE[H]BBBB[B]EE[W]B
len: 1328, ms: [576, 576, 176], score: 146.30, avg: 0.110166, str: BBBB[B]EEE[bH]EEE[B]BBBB[sH]BBBB[B]EEE[bH]YYYY[W]YYYYY[B]BBBBBBB[bB]BBB
len: 1288, ms: [576, 576, 136], score: 144.10, avg: 0.111879, str: BBBB[B]BBBBB[M]EEEEEE[bH]YYYY[W]YYYYY[bB]EEE[H]BBBB[B]BBB[W]B
len: 1312, ms: [512, 576, 224], score: 148.20, avg: 0.112957, str: BBBB[B]YY[H]YY[B]YY[H]YY[B]BBBBBBB[B]EEEE[sM]EEEE[V]BBBBBBB[F]E[B]BBBBBBB[bB]BBB
len: 1280, ms: [576, 576, 128], score: 146.20, avg: 0.114219, str: BBBB[B]YYYY[B]BBBBBBB[B]EEEE[sM]EEEEE[W]B[W]YYYYY[B]BBBB[sH]
len: 1296, ms: [576, 576, 144], score: 158.00, avg: 0.121914, str: BBBB[B]EEEE[M]BBBB[F]B[I]EEEEE[M]BBBBBB[W]YYYYY[B]YYYY[B]BBB
len: 1296, ms: [576, 576, 144], score: 158.00, avg: 0.121914, str: BBBB[B]YYYY[B]YYYYY[M]BBBBBB[W]EEEEE[F]B[I]BBBB[W]EEEE[B]BBB
len: 1288, ms: [576, 576, 136], score: 159.00, avg: 0.123447, str: EEE[B]E[V]BBBB[M]BBBB[F]E[B]EEEE[M]BBBBBB[W]YYYYY[B]YYYY[B]BBB
Search completed in 3.364357029s
```

3.4s is not bad!  And those compositions are pretty decent too.  You can see how the call weighting
is promoting compositions which make clever use of few calls.

You can use [`to-complib.py`](to-complib.py) to generate a calling string to paste into CompLib's
composition input spreadsheet.  The string will be automatically copied to your system's clipboard.
For example, running `./to-complib.py
BBBB[B]YYYY[B]YYYYY[M]BBBBBB[W]E[sH]BBBB[B]E[V]BBBBBBB[F]E[bB]EEE[H]BBBB[B]EE[W]B` will copy this
into your clipboard:
```text
M	F	Methods	V	B	H	W
				-
				-
-
						-
					s
				-
			-
	-
				b
					-
				-
						-
		BBBBYYYYYYYYYBBBBBBEBBBBEBBBBBBBEEEEBBBBEEB
```

When pasted into CompLib, this gives
[this comp](https://complib.org/composition/90918?accessKey=88cedf2b68369eb0987a752ca5b17bc76931eb8c).

---

## Quick List of Parameters

Here's a short list of the parameters, along with their default value.  Clicking each value will
take you to more in-depth docs about it.

**General:**
- [`length`](#length-required)
- [`num_comps = 100`](#num_comps)
- ~~[`allow_false = false`](#allow_false)~~ _(removed in v0.13.0)_
- [`require_truth = true`](#require_truth)
- ~~[`queue_limit`](#queue_limit)~~ _(removed in v0.12.0)_
- [`graph_size_limit`](#graph_size_limit)

**Methods:**
- [`method`](#method)
- [`methods`](#methods-2)
- [`method_count`](#method_count) (default to ±10% balance)
- [`splice_style = "leads"`](#splice_style)
- [`splice_weight = 0.0`](#splice_weight) _(since v0.7.0)_
- [`atw_weight`](#atw_weight) _(since v0.14.0)_
- [`require_atw = false`](#require_atw) _(since v0.14.2)_

**Calls:**
- [`base_calls = "near"`](#base_calls)
- [`bobs_only = false`](#bobs_only-and-singles_only) _(since v0.6.0)_
- [`singles_only = false`](#bobs_only-and-singles_only) _(since v0.6.0)_
- [`bob_weight = -1.8`](#bob_weight-and-single_weight)
- [`single_weight = -2.3`](#bob_weight-and-single_weight)
- [`calls = []`](#calls-2)

**Music:**
- ~~[`default_music = true`](#default_music)~~ _(since v0.8.0, replaced by `base_music` in v0.9.0)_
- [`base_music = "default"`](#base_music) _(since v0.9.0)_
- [`music_file`](#music_file) (optional)
- [`music = []`](#music-2)
- [`start_stroke = "back"`](#start_stroke)

**Courses:**
- [`part_head = ""`](#part_head) (i.e. default to 1-part)
- [`split_tenors = false`](#split_tenors)
- [`courses`](#courses) (default determined by `split_tenors`, _renamed from `course_heads` in v0.13.0)_
- [`course_weights = []`](#course_weights) _(renamed from `ch_weights` in v0.13.0)_
- [`handbell_coursing_weight = 0`](#handbell_coursing_weight)
- ~~[`leadwise`](#leadwise) (default set by Monument)~~ _(removed in v0.10.0)_
- [`non_duffer_courses`](#non_duffer_courses) _(added in v0.12.0)_
- [`max_total_duffer`](#max_total_duffer-and-max_contiguous_duffer) _(added in v0.12.0)_
- [`max_contiguous_duffer`](#max_total_duffer-and-max_contiguous_duffer) _(added in v0.12.0)_

**Starts/Ends:**
- [`start_row = ""`](#start_row-and-end_row) _(since v0.10.0.  "" is equivalent to rounds)_
- [`end_row = ""`](#start_row-and-end_row) _(since v0.10.0.  "" is equivalent to rounds)_
- [`snap_start = false`](#snap_start)
- [`start_indices`](#start_indices-and-end_indices) (default set by `snap_start`)
- [`end_indices`](#start_indices-and-end_indices) (default to allow any finish)


------

## More Detail on Parameters

### General

#### `length` (required)

Determines the _inclusive_ range of lengths into which the compositions must fit.  This can take the
following forms:

```toml
length = { min = 600, max = 700 } # require length of 600-700 rows (inclusive)
length = 1729        # require exact length

length = "practice"  # equivalent to `{ min =    0, max =  300 }`
length = "QP"        # equivalent to `{ min = 1250, max = 1350 }`
length = "half peal" # equivalent to `{ min = 2500, max = 2600 }`
length = "peal"      # equivalent to `{ min = 5000, max = 5200 }`
```

#### `num_comps`

The number of compositions you want.  Defaults to `100`

#### `allow_false`

**_(replaced by `require_truth` in v0.13.0)_**

If `true`, Monument will ignore falseness and generate potentially false compositions.  Defaults to
`false`.

#### `queue_limit`

**_(since v0.11.0)_**

Sets a limit on the number of compositions that Monument will consider at any time.  Monument's
memory usage is proportional to this queue's length.  Defaults to 10 million.

#### `graph_size_limit`

**_(since v0.11.0)_**

Sets a limit on the number of chunks in the composition graph.  Defaults to 100,000.

### Methods

#### `method`

Specifies a single method for the composition:

```toml
method = "Bristol Surprise Major"

# or

[method]
title = "Lincolnshire Surprise Major"
shorthand = "N" # (optional; defaults to the first letter of the title)
labels = { LE = 0, HL = 16 } # (optional; defaults to `{ LE = 0 }`)
lead_locations = { LE = 0, HL = 16 } # (pre-v0.11.0 name for `labels`)
# Overrides for global values (all optional):
count = { min = 224, max = 600 }
courses = ["*78"]
start_indices = [2]
end_indices = [2]

# or

[method]
name = "Double Norwich Court" # Note this is *name*, not *title*
place_notation = "x4x36x5x8,8"
stage = 8
shorthand = "N" # (optional; defaults to the first letter of the title)
labels = { LE = 0, HL = 8 } # (optional; defaults to `{ LE = 0 }`)
lead_locations = { LE = 0, HL = 16 } # (pre-v0.11.0 name for `labels`)
# Overrides for global values (all optional):
count = { min = 224, max = 600 }
courses = ["*78"]
start_indices = [2]
end_indices = [2]
```

You can also specify multiple indices for the same `label`, useful for e.g. Stedman:

```toml
[method]
title = "Stedman Triples"
labels = { SE = [3, 9] }
```

#### `methods`

Same as `method`, but takes a list of methods:

```toml
methods = [
    "Bristol Surprise Major",
    { title = "Lincolnshire Surprise Major", shorthand = "N" },
    { name = "Bastow", place_notation = "x2,1", stage = 8 },
]
```

#### `splice_style`

Determines how methods can be spliced.  Has no effect for single-method compositions.  Options:
```toml
splice_style = "leads"          # (default; change method at every defined lead location)
# or
splice_style = "calls"          # only change method when a call does happen
```

Before `v0.10.0`, `splice_style = "call locations"` was possible and would only add splices where a
call _could have_ been made (even if it wasn't).

(Note that, for versions before `v0.8.0`, `splice_style = "call locations"` and `splice_style =
"calls"` will both allow splices over part heads.  This is fixed in `v0.8.0` and later)

#### `method_count`

Min-max limits on how many rows of each method is allowed.  Defaults to ±10% method balance.
```toml
method_count.min = 0 # Allow Monument to ignore methods, but keep the default maximum
# or
method_count = { min = 100, max = 300 } # Force a given method count range
```

#### `splice_weight`

**_(since v0.7.0)_**

Weight applied to each change of method.  Positive values will encourage more c.o.m.; negative
values will encourage few c.o.m.  Defaults to 0 (i.e. don't care about c.o.m.).

#### `atw_weight`

**_(since v0.14.0)_**

Weight applied to a composition that is 'all the work' (i.e. one where every bell rings every part
of every place bell of every method).  The score generated is continuous - a 'half-the-work'
composition will be given an additional score of `atw_weight * 0.5`.

Setting this to any value (even `0.0`) will cause the atw-ness of compositions to be displayed.

**NOTE:** Monument's current search algorithm finds it quite challenging to actually find fully atw
compositions.  The algorithm is designed to generate 'all-rounder' compositions, and therefore
struggles with 'all-or-nothing' metrics like atw.

#### `require_atw`

**_(since v0.14.2)_**

If `require_atw = true`, then the only fully atw compositions will be outputted.  Defaults to
`false` (i.e. emit all compositions, even if they are not atw).

### Calls

#### `base_calls`

Lead-end calls which are automatically generated:
```toml
base_calls = "near" # default; 14 bob and 1234 single
# or
base_calls = "far"  # 1(n-2) bob and 1(n-2)(n-1)n single
# or
base_calls = "none" # no base calls, only what you've added
```

#### `bobs_only` and `singles_only`

**_(since v0.6.0)_**

If either are `true`, then `base_calls` will only generate that call type.  Setting both `bobs_only`
and `singles_only` to `true` makes no sense and causes an error. By default, both bobs and singles
are generated.

#### `bob_weight` and `single_weight`

Sets the score given to the bob/single generated by `base_calls`.  Defaults to `bob_weight = -1.8`,
`single_weight = -2.5`.

#### `calls`

Array of custom calls:
```toml
[[calls]]
place_notation = "16"
symbol = "x"
label = "LE"          # Optional; where in the method to apply the call.  Defaults to "LE"
lead_location = "LE"  # Optional; pre-v0.11.0 name for `label`
weight = -4           # Optional; Score given to each instance of this call.  Defaults to -3
calling_positions = "LIBFVXSMWH" # Optional; defaults to 'LIBFVXSEN...' with 'MWH' added
```

> ##### Note: The (obselete) `debug_symbol` parameter
>
> Up to v0.12.0, calls had a `debug_symbol` which specified the 'long' symbol used when displaying
> the call.  For example, a custom bob would be specified like:
>
> ```
> [[calls]]
> symbol = ""
> debug_symbol = "-"
> place_notation = "12"
> ```
>
> In v0.13.0, this was removed and is calculated automatically, so a custom bob simply needs a symbol
> of `-`:
>
> ```
> [[calls]]
> symbol = "-"
> place_notation = "12"
> ```

> ##### Note: Using different calls in different methods
>
> Since _v0.9.0_, calls can go from/to different lead `labels`.  This is useful if, for example, you
> want to make sure you only apply some calls to some methods.  The following example adds `16` bobs
> only in 8ths place methods, and `14` bobs in 2nds place methods (as in
> [Leary's 23](https://complib.org/composition/21607)):
>
> ```toml
> length = "QP"
> methods = [
>     { title = "Bristol Surprise Major",     labels = { LE = 0, 8ths = 0 } },
>     { title = "Deva Surprise Major",        labels = { LE = 0, 8ths = 0 } },
>     { title = "Cambridge Surprise Major",   labels = { LE = 0, 2nds = 0 } },
>     { title = "Superlative Surprise Major", labels = { LE = 0, 2nds = 0 } },
> ]
> part_head = "13456782"
>
> base_calls = "none" # Only use our own custom calls
> [[calls]]
> symbol = ""
> debug_symbol = "-"
> place_notation = "14"
> # '14' bobs go *from* only 2nds place methods, but *to* any method
> label = { from = "2nds", to = "LE" }
>
> [[calls]]
> symbol = "x"
> place_notation = "16"
> # '16' bobs go *from* only 8ths place methods, but *to* any method
> label = { from = "8ths", to = "LE" }
> ```
>
> Notice how we're using lead labels `2nds` and `8ths` to control which calls are able to be placed at
> the end of a lead of each method.  Also note how all calls lead to `LE`, which means that any method
> can follow any call (if the calls didn't change lead location, then 2nds/8ths place methods couldn't
> be spliced over a call).

### Music

#### `default_music`

**_(since v0.8.0, replaced by `base_music` in v0.9.0)_**

See `base_music`:
- `default_music = false` is equivalent to `base_music = "none"`
- `default_music = true` is equivalent to `base_music = "default"`

#### `base_music`

Like [`base_calls`](#base_calls), `base_music` adds a set of music definitions to the search.
`base_music` has three values, the default being `base_music = "default"`:

1. `base_music = "none"`:  Adds no music to the search; the only music will be what you explicitly
   specify.
2. `base_music = "default"`:  For most stages, this adds a basic music profile intended to be a sane
   default for most music tastes.  This roughly follows the 'headline' music in CompLib (i.e. the
   summary line shown below every composition.

   The default music profiles are equivalent to importing the following music files:
   - [Minor](src/music/default-music-minor.toml)
   - [Triples](src/music/default-music-triples.toml)
   - [Major](src/music/default-music-major.toml)
   - [Royal](src/music/default-music-royal.toml)
   - [Maximus](src/music/default-music-maximus.toml)
3. `base_music = "complib"` _(since v0.14.0)_: For most stages, this adds a music profile that follows the complib music
   scoring (without the scoring for wraps included currently).

   The complib music profiles are equivalent to importing the following music files:
   - [Minor](src/music/complib-music-minor.toml)
   - [Triples](src/music/complib-music-triples.toml)
   - [Major](src/music/complib-music-major.toml)
   - [Caters](src/music/complib-music-caters.toml)
   - [Royal](src/music/complib-music-royal.toml)
   - [Cinques](src/music/complib-music-cinques.toml)
   - [Maximus](src/music/complib-music-maximus.toml)

#### `music_file`

Relative path to a file containing music definitions (i.e. a single `music` array).

#### `music`

Array of custom music types:
```toml
[[music]]
run_lengths = [4, 5, 6, 7, 8] # or a single length: `run_length = 4`
internal = true               # Optional; defaults to `false`
# or
patterns = ["*6578", "6578*"]       # or a single pattern: `pattern = "*5x6x7x8*"`
count_each = { min = 12, max = 24 } # Count range applied per-pattern.
                                    # Optional; defaults to allowing anything
# or
preset = "crus"
# or
preset = "5678 combinations"
# or
preset = "near misses"

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

#### `start_stroke`

The stroke of the first non-rounds row (technically, the first row that isn't `start_row`).
```toml
start_stroke = "hand" # Default
# or
start_stroke = "back"
```
Before v0.10.0, this was reversed (i.e. the opposite way round to what people expect).

### Courses

#### `part_head`

A row which determines which part heads will be generated.  Note that Monument can generate
compositions with a different part head, provided the same set of parts are generated (so
`part_head = "23456781"` and `part_head = "81234567"` are equivalent but `part_head = "56781234"` is
not).  Defaults to rounds (i.e. one part, or `part_head = ""`).

#### `courses`

List of masks which define the courses that Monument can use.  Defaults to tenors together, or any
course (if `split_tenors` is set).  For example:
```toml
courses = ["*78", "xxxx7856", "12345xxx"]
```

#### `split_tenors`

If `courses` isn't specified, this lets Monument use any courses, as opposed to just those with
the tenors together.  On higher stages, this will almost certainly cause Monument's graph size limit
to be reached.  Defaults to `false`.

#### `course_weights`

Applies some score to every row in a course which contains a lead head which matches a given mask.
For example, the following will weight Monument to create compositions where handbell pairs are
coursing often:
```toml
[[course_weights]]
patterns = [
    "*78",
    "*56", "*65",
    "*34", "*43",
] # can also use e.g. `pattern = "*78"`
weight = 0.05 # this is small because the weight is applied per row
```

#### `handbell_coursing_weight`

Generates `course_weights` which apply the given weight to every row where a handbell pair coursing
(this score gets multiplied for courses with multiple handbell pairs coursing).  Equivalent to
something like this (truncated according to stage):
```toml
[[course_weights]]
patterns = [
    "*12", "*21",
    "*34", "*43",
    "*56", "*65",
    "*78", "*87",
    "*90", "*09",
        ...
]
```
Defaults to 0.

#### `leadwise`

**_(removed in v0.10.0)_**

If set, this will stop Monument using calling positions, and instead label all the calls
positionally.  `courses`, `split_tenors` and `course_weights` will obviously have no effect, and
this will always generate split-tenors compositions.  You should rarely have to set this yourself; by
default, Monument will set this automatically if the tenor is affected by the part head (e.g. in
cyclic) but otherwise will stick to course-wise compositions.  The only times you're likely to need
this is for weird cases like differential methods, which don't have a well-defined concept of a
'course head'.

#### `non_duffer_courses`

Specifies which courses are 'non-duffer'.  Courses which don't satisfy anything in
`non_duffer_courses` are considered 'duffers' and `max_{total,contiguous}_duffer` can be used to
restrict how much 'duffer' is allowed in a given composition.

Shorthands for `any_bells` and `any_stroke` can be used to easily specify lots of courses based on
some pattern.  For example, all the following specify the 4-bell run courses in most Major methods:

1. Specifying all courses explicitly:
   ```toml
   non_duffer_courses = [
       "*5678", "*8765", "*6587", "*7856",
       "*4567", "*7654", "*5476", "*6745",
       "*3456", "*6543", "*4365", "*5634",
       "*2345", "*5432", "*3254", "*4523",
   ]
   ```
2. Using `any_bells` to expand the courses with every similar pattern of bells:
   ```toml
   non_duffer_courses = [
       { courses = ["*5678", "*8765", "*6587", "*7856"], any_bells = true },
   ]
   ```
3. Using `any_stroke` to expand music on backstroke to that on handstroke:
   ```toml
   non_duffer_courses = [
       { courses = ["*5678", "*8765"], any_bells = true, any_stroke = true },
   ]
   ```

#### Quick note on `non_duffer_courses` and multi-parts

Currently, Monument does not consider the non-duffer status of each part separately.  Thus, a
'chunk' of ringing is a non-duffer if and only if it's a non-duffer in **every** part.  This is a
potential gotcha when combined with e.g. cyclic multi-parts.  For example, if you want
'non-duffers' to be courses which generate 4-bell runs in _some_ part, the first instinct is often
to specify 4-bell run courses like so:

```toml
non_duffer_courses = [{ courses = ["*5678", "*8765"], any_bells = true, any_stroke = true }]
```

This, however, makes only the plain course a non-duffer.  Any other course will generate `*6782` in
some part and therefore all other 4-bell run courses are a duffer in some part.  To get the
expected behaviour, you need to explicitly add `*6782`, `*7823` and `*8234` as non-duffers, like so:

```toml
non_duffer_courses = [
  { courses = ["*5678", "*8765"], any_bells = true, any_stroke = true },
  { courses = ["*6782", "*7823", "*8234"], any_stroke = true } # Not using `any_bells` here
]
```

#### `max_total_duffer` and `max_contiguous_duffer`

`max_total_duffer` and `max_contiguous_duffer` specify limits (in number of rows) on how much
'duffer' can be rung.  For example, the following produces QPs of Bristol with at most two leads of
_consecutive_ duffer and at most four leads of _total_ duffer:

```toml
length = "qp"
method = "Bristol Surprise Major"
bobs_only = true

non_duffer_courses = [
    { courses = [ "*5678", "*5x678", "*8765", "*8x765" ], any_stroke = true, any_bells = true },
    "*6578", "*5x678",
]
max_contiguous_duffer = 64 # Limited by contiguous duffers
max_total_duffer = 128
```

### Starts/Ends

#### `start_row` and `end_row`

**_(since v0.10.0)_**

Specifies the row used to start or finish the composition, both defaulting to rounds.  These don't
work with multi-parts, but are useful for things like getting Monument to extend 720s of Minor into
a quarter-peal length.

#### `snap_start`

If no `start_indices` have been set, `snap_start = true` allows just snap starts (i.e. is equivalent
to `start_indices = [2]`).  Defaults to `false` (i.e. just lead end starts).

#### `start_indices` and `end_indices`

Sets the indices within the lead where the composition can start/end.  The default value of
`start_indices` is determined by `snap_start`, whereas the `end_indices` defaults to allowing any
value. These indices are taken modulo the lead length and can be negative, so for example 2, -30 and
34 would all refer to the backstroke snap in treble dodging Major.

---

### That's all, folks.  Happy composing!
