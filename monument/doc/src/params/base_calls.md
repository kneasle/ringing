# `base_calls`

**Defaults to `"near"` (`14` bobs, `1234` singles at lead ends).**

Almost all compositions use the a small set of standard bobs and singles: `14` bobs and `1234`
singles, or `1(n-2)` bobs and `1(n-2)(n-3)(n)` singles, always at lead ends).

Therefore, Monument has a shorthand for generating these, using the `base_calls` parameter:

```toml
base_calls = "near" # default; 14 bob and 1234 single (14n/1234n for odd-bell methods)
# or
base_calls = "far"  # 1(n-2) bob and 1(n-2)(n-1)(n) single
# or
base_calls = "none" # no base calls, only what you've added
```

These can be customised with these other parameters:

- [`bobs_only`](bobs_only.md)
- [`singles_only`](singles_only.md)
- [`bob_weight`](bob_weight.md)
- [`single_weight`](single_weight.md)
