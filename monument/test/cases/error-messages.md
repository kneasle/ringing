# Error Message Test Suite

Each section in this file corresponds to one test case and must include:
- A sequence of nested headers, which set the name of the test (e.g. `## method-pn-parsing` and
  `### repeated-place` will be named `method-pn-parsing/repeated-place`)
- A fenced code region with type `toml` (this is the input file).  Optionally may include a second
  `toml` region, which is interpreted as a music file.
- A fenced plain-text region with no type (i.e. type `text`).  This contains the expected output
  with any ANSI escape sequences removed.

Using a markdown file allows us to conveniently embed many small files in one larger file, whilst
still allowing syntax highlighting to work correctly.  Markdown can still be parsed easily and
quickly whilst still being widely-used, flexible and human-readable.



## method-pn-parsing

### repeated-place
```toml
length = "QP"
[method]
name = "Bristol"
place_notation = "&x5x4.5x5.36.4x4.585x4x1,+9" # 5 is repeated twice in `585`
stage = 8
```

### misplaced-plus
```toml
length = "QP"
[method]
name = "Bristol"
place_notation = "&x5+4.5x5.36.4x4.5x4x1,+8" # first + shouldn't be there
stage = 8
```

### ambiguous-gap
```toml
length = "QP"
[method]
name = "Bristol"
place_notation = "&x15x4.5x5.36.4x4.5x4x1,+8" # `15` has an ambiguous gap
stage = 8
```

### odd-stage-cross
```toml
length = "QP"
[method]
name = "Bristol"
place_notation = "&x15x4.5x5.36.4x4.5x4x1,+8" # Can't use 'x' in an odd stage
stage = 7
```

### bell-out-of-stage
```toml
length = "QP"
[method]
name = "Bristol"
place_notation = "&x5x4.5x5.36.4x4.5x4x1,+9" # 9 is too big
stage = 8
```



## method-not-found

### case-1
```toml
length = "QP"
method = "Brisol Suprise Major"
```

### case-2
```toml
length = "QP"
method = "Camibridge Surprise Manor"
```

### case-3
```toml
length = "QP"
method = "Corwnall Surprise major" # TODO: Maybe we shouldn't display a diff for the lowercase 'm'
```



## no-methods
```toml
length = "QP"
```



## part-head-parse

### 1
```toml
length = "QP"
method = "Bristol Surprise Major"
part_head = "13" # 2 is missing
```

### 2
```toml
length = "QP"
method = "Bristol Surprise Major"
part_head = "1321" # 1 is duplicated
```

### 3
```toml
length = "QP"
method = "Bristol Surprise Major"
part_head = "123456789" # 9 is out of stage Major
```



## duplicate-shorthand
```toml
length = "QP"
methods = ["Lessness Surprise Major", "London Surprise Major"] # Both would be named 'L'
```

## undefined-lead-location
```toml
length = "QP"
method = "Bristol Surprise Major"
[[calls]]
symbol = "x"
place_notation = "16"
lead_location = "poo" # poo isn't defined anywhere
```

## ambiguous-course-heads
```toml
length = "QP"
method = "Bristol Surprise Major"
course_heads = ["*56", "*78"]
```

## calling-positions-too-short
```toml
length = "QP"
method = "Bristol Surprise Major"
[[calls]]
symbol = "x"
place_notation = "16"
calling_positions = "LIOFVMW" # No 'H'
```

## bobs-and-singles-only
```toml
length = "QP"
method = "Bristol Surprise Major"
bobs_only = true
singles_only = true
```

## call-pn-parse
```toml
length = "QP"
method = "Bristol Surprise Major"
[[calls]]
symbol = "x"
place_notation = "10" # ERROR!
```

## ch-mask

### too-short
```toml
length = "QP"
method = "Bristol Surprise Major"
course_heads = ["*78", "12345"]
```

### too-long
```toml
length = "QP"
method = "Bristol Surprise Major"
course_heads = ["*78", "1234x5678"]
```

### bell-out-of-stage
```toml
length = "QP"
method = "Bristol Surprise Major"
course_heads = ["*78", "1234*9"]
```

### too-many-*s
```toml
length = "QP"
method = "Bristol Surprise Major"
course_heads = ["*78", "1*2*3"]
```



## ch-pattern

### too-short
```toml
length = "QP"
method = "Bristol Surprise Major"
[[ch_weights]]
pattern = "12345"
weight = 0.1
```

### too-long
```toml
length = "QP"
method = "Bristol Surprise Major"
[[ch_weights]]
pattern = "1234*5678x"
weight = 0.1
```

### bell-out-of-stage
```toml
length = "QP"
method = "Bristol Surprise Major"
[[ch_weights]]
pattern = "x9*"
weight = 0.1
```

### too-many-*s
```toml
length = "QP"
method = "Bristol Surprise Major"
[[ch_weights]]
pattern = "*7*8"
weight = 0.1
```



## duplicate-calls

### same-pn

This doesn't actually produce an error message, because there are two definitions of the same call
(1234 LE single) which Monument will deduplicate:

```toml
length = "practice"
method = "Bristol Surprise Major"
base_music = "none"

[[calls]]
symbol = "s" # Not a clash, because this is an identical definition
place_notation = "1234"
```

### different-lead-locations

This doesn't actually produce an error message, because 's' is defined once on the lead-end and once
on the half-lead:

```toml
length = "practice"
method.title = "Bristol Surprise Major"
method.lead_locations = { LE = 0, HL = 16 }
base_music = "none"

[[calls]]
symbol = "s" # Not a clash, because this is a HL call
place_notation = "5678"
lead_location = "HL"
calling_positions = "hwmvfbil"
```

### display-symbol
```toml
length = "QP"
method = "Bristol Surprise Major"

[[calls]]
debug_symbol = "z"
symbol = "s" # Name clashes with the builtin 1234 singles
place_notation = "1678"
```

### debug-symbol
```toml
length = "QP"
method = "Bristol Surprise Major"

[[calls]]
symbol = "-" # Name clashes with the builtin 14 bob
place_notation = "16"
```

## chs-not-in-other-parts

```toml
length = "peal"
method = "Bristol Surprise Major"
part_head = "134265"
course_heads = ["*78", "*7856"] # `*7856` becomes `*7865` in other parts
```
