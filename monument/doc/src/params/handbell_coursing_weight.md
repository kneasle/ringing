# `handbell_coursing_weight`

**Defaults to `0`.**

Generates [`course_weights`](course_weights.md) which apply the given weight to every row where a
handbell pair coursing (this score gets multiplied for courses with multiple handbell pairs
coursing).  Equivalent to something like this (truncated according to stage):

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
