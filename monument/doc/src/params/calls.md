# `calls`

**Defaults to `[]` (i.e. no custom calls).**

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

Since _v0.9.0_, calls can go from/to different lead `labels`.  This is useful if, for example, you
want to make sure you only apply some calls to some methods.  The following example adds `16` bobs
only in 8ths place methods, and `14` bobs in 2nds place methods (as in
[Leary's 23](https://complib.org/composition/21607)):

```toml
length = "QP"
methods = [
    { title = "Bristol Surprise Major",     labels = { LE = 0, 8ths = 0 } },
    { title = "Deva Surprise Major",        labels = { LE = 0, 8ths = 0 } },
    { title = "Cambridge Surprise Major",   labels = { LE = 0, 2nds = 0 } },
    { title = "Superlative Surprise Major", labels = { LE = 0, 2nds = 0 } },
]
part_head = "13456782"

base_calls = "none" # Only use our own custom calls
[[calls]]
symbol = "-"
place_notation = "14"
# '14' bobs go *from* only 2nds place methods, but *to* any method
label = { from = "2nds", to = "LE" }

[[calls]]
symbol = "x"
place_notation = "16"
# '16' bobs go *from* only 8ths place methods, but *to* any method
label = { from = "8ths", to = "LE" }
```

Notice how we're using lead labels `2nds` and `8ths` to control which calls are able to be placed at
the end of a lead of each method.  Also note how all calls lead to `LE`, which means that any method
can follow any call (if the calls didn't change lead location, then 2nds/8ths place methods couldn't
be spliced over a call).
