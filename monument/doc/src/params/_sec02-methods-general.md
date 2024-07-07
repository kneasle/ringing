# General Method-Related Parameters

These parameters affect all methods and so are put in the root level of the file, rather than
inside of `method` or `methods`.

**Contents:**
- [`splice_style`](#splice_style)
- [`method_count`](#method_count)
- [`splice_weight`](#splice_weight)

---



## `splice_style`

Determines how methods can be spliced.  Has no effect for single-method compositions.  Options:

```toml
splice_style = "leads"  # (default; change method at every defined lead location)
# or
splice_style = "calls"  # only change method at calls
```

Before `v0.10.0`, `splice_style = "call locations"` was possible and would only add splices where a
call _could have_ been made (even if it wasn't).



## `method_count`

Min-max limits on how many rows of each method is allowed.  Defaults to ±10% method balance.
```toml
method_count.min = 0 # Allow Monument to ignore methods, but keep the default maximum
# or
method_count = { min = 100, max = 300 } # Force a given method count range
```



## `splice_weight`

**_(since v0.7.0)_**

Weight applied to each change of method (c.o.m.).

Positive values will encourage more c.o.m.; negative values will encourage few c.o.m.

Defaults to `0` (i.e. be impartial about c.o.m.).
