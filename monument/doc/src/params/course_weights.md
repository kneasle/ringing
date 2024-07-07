# `course_weights`

**Defaults to `0`.**

Applies a score to every row in a course which contains a lead head which matches a given mask.

For example, the following will weight Monument to create compositions where handbell pairs are
coursing often (though this could be done more succinctly with
[`handbell_coursing_weight = 0.05`](handbell_coursing_weight.md)):

```toml
[[course_weights]]
patterns = [
    "*78",
    "*56", "*65",
    "*34", "*43",
] # can also use e.g. `pattern = "*78"`
weight = 0.05 # this is small because the weight is applied per row
```
