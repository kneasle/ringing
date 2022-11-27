# General Parameters

These parameters specify 'general' properties of the search, not belonging in any specific category.

These are generally used very rarely - with the exception of `length`, which must be in
every search.

**Contents:**
- [`length`](#length)
- [`num_comps`](#num_comps)
- [`allow_false`](#allow_false)
- [`queue_limit`](#queue_limit)
- [`graph_size_limit`](#graph_size_limit)

---



## `length`

`length` is the only parameter which is absolutely required (Monument also needs you to specify at
least one method, but methods can be added with either the `method` or `methods` parameters).

`length` determines the _inclusive_ range of lengths into which the compositions must fit.  This
can take the following forms:

```toml
length = { min = 600, max = 700 } # require length of 600-700 rows (inclusive)
length = 1729        # require an exact length

length = "practice"  # equivalent to `{ min =    0, max =  300 }`
length = "QP"        # equivalent to `{ min = 1250, max = 1350 }`
length = "half peal" # equivalent to `{ min = 2500, max = 2600 }`
length = "peal"      # equivalent to `{ min = 5000, max = 5200 }`
```



## `num_comps`

The number of compositions that Monument will emit before finishing the search.

Monument tends to generate compositions in decreasing order of goodness, so the _first_ `num_comps`
compositions found will correspond roughly to the _best_ `num_comps` compositions.

Defaults to `100`.

**Example:**

```toml
length = "QP"
method = "Yorkshire Surprise Major"
num_comps = 5 # Only emit the first five compositions
```



## `allow_false`

If `true`, Monument will ignore falseness and generate potentially false compositions.

Defaults to `false` (i.e. require truth).



## ~~`queue_limit`~~

**_(since v0.11.0, removed in v0.12.0)_**

Sets a limit on the number of compositions that Monument will consider at any time.  Monument's
memory usage is proportional to this queue's length.

In v0.12.0, this was removed in favour of an explicit memory limit, set with the `--mem-limit` CLI
argument and defaulting to 80% of available memory.

Defaults to 10,000,000.



## `graph_size_limit`

**_(since v0.11.0)_**

Sets a limit on the number of chunks in the composition graph.

Defaults to 100,000.
