# `method_count`

**Defaults to ±10% method balance.**

Min-max limits on how many rows of each method is allowed.

```toml
method_count.min = 0 # Allow Monument to ignore methods, but keep the default maximum
# or
method_count = { min = 100, max = 300 } # Force a given method count range
```

This can be overridden per-method using [the `count_range` parameter](method/count_range.md).
