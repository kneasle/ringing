# Defining Calls

These parameters specify which calls will be used in generated compositions.

**Contents:**
- [`base_calls`](#base_calls)
- [`bobs_only` and `singles_only`](#bobs_only-and-singles_only)
- [`bob_weight` and `single_weight`](#bob_weight-and-single_weight)
- [`calls` array:](#calls)
  - [`place_notation`](#place_notation)
  - [`symbol`](#symbol)
  - [`label`](#label)
  - [`weight`](#weight)
  - [`calling_positions`](#calling_positions)

---



## `base_calls`

Almost all compositions use the same bobs and singles (`14` bobs and `1234` singles), so Monument
will add these automatically so you don't have to write them out for every single search.  However,
some compositions want either far calls or fully custom calls, and you can specify these with the
`base_calls` parameter.

Lead-end calls which are automatically generated:
```toml
base_calls = "near" # default; 14 bob and 1234 single (14n/1234n for odd-bell methods)
# or
base_calls = "far"  # 1(n-2) bob and 1(n-2)(n-1)(n) single
# or
base_calls = "none" # no base calls, only what you've added
```



## `bobs_only` and `singles_only`

**_(since v0.6.0)_**

If either are `true`, then `base_calls` will only generate that call type.  Setting both `bobs_only`
and `singles_only` to `true` makes no sense and causes an error.

Both default to `false` - i.e. generating both bobs and singles.



## `bob_weight` and `single_weight`

Sets the score given to the bob/single generated by `base_calls`.  Note that this does not affect
calls which you have made explicitly in the [`calls` array](#calls)

Defaults to `bob_weight = -1.8`, `single_weight = -2.5`.



## `calls`

**TODO:** Write descriptions for each parameter

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
