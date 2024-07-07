# `splice_style`

**Defaults to `"leads"`.**

Determines how methods can be spliced.  Has no effect for single-method compositions.

Options:

```toml
splice_style = "leads"  # (default; change method at every defined lead location)
# or
splice_style = "calls"  # only change method at calls
```
