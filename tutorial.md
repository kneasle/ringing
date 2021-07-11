# Tutorial

So, you want to get your computer to generate compositions for you?  Luckily, you've come to the
right place!  This tutorial goes through a simple step-by-step example (generating QPs of Yorkshire
Major).  Let's jump in and generate some compositions!

## Creating an input file

Suppose that we are searching for quarter peals of Yorkshire Surprise Major, and we like runs of at
least 4 bells and prefer runs of 5678 over other types of run.

One of the design goals of Monument is to have sane defaults.  For example, Monument will default to
only generate strictly tenors-together compositions, which is what most people want.  Monument also
defaults to outputting the 30 best compositions, which is a fine for this example.

The tenors-together restriction can be overridden with either `split_tenors = true` or by specifying
`course_heads` manually (although Monument will nearly always be horribly slow for fully
split-tenors searches - you'd be better off using SMC).  Likewise, the number of compositions output
can be set with `num_comps = <number>`.  All of these options go at the top of the file in the
'general' section.

### Length

In order to run Monument, we first need to write an input `.toml` file to explain to Monument what
we want.  For this example, I'll call the file `yorkshire-s8-qps.toml`.  Now lets get started with
our specification: we want quarter peals, so let's start our file with:

> _yorkshire-s8-qps.toml:_
```toml
length = "QP"
```

Setting the length to `"QP"` corresponds to the range 1250 to 1350 (inclusive), but we could be more
specific with `length = { min = 1250, max = 1280 }`, or specify exactly one length with
`length = 1280`.

### Method

Now that we have a length, we need to specify the method.  We do this by setting the `method`
attribute, by putting our values under a header of `[method]`.  We can't load methods from the
Central Council's library (yet!), so we have to input Yorkshire's place notation manually:

> _yorkshire-s8-qps.toml:_
```toml
length = "QP"

# NEW:
[method]
stage = 8
place_notation = "x38x14x58x16x12x38x14x78,12"
```

### Calls

The final step that's absolutely necessary is to define which calls we are using.  These are fairly
obviously _near_ calls (i.e. `14` bobs and `1234` singles), but Monument doesn't want to try being
clever because guessing the calls will sometimes be wrong (and cause lots of tedious debugging).
We could specify the calls manually with `calls = [...]`, but near and far calls are nearly
ubiquitous and Monument has a shorthand (`base_calls`) for them:

> _yorkshire-s8-qps.toml:_
```toml
length = "QP"
base_calls = "near" # NEW

[method]
stage = 8
place_notation = "x38x14x58x16x12x38x14x78,12"
```

### Music

At this point, the file is complete enough that we could give it to Monument.  However, there is a
problem: we haven't told Monument what makes a 'good' composition.  Therefore, Monument will simply
output the first 30 compositions it generates.  To fix this, we need to specify what music we care
about:

> _yorkshire-s8-qps.toml:_
```toml
length = "QP"
base_calls = "near"

[method]
stage = 8
place_notation = "x38x14x58x16x12x38x14x78,12"

# ===== NEW: =====
[[music]]
run_lengths = [4, 5, 6, 7]
# weight defaults to 1.0

[[music]]
patterns = ["*5678", "*8765", "5678*", "8765*", "*6578"]
weight = 0.5
```

Note that we give 5678 patterns a weight of 0.5, since most of them already count as 4-bell runs
(and therefore have an effective score of 1.5).

The `music` attribute is an array of music definitions, and in TOML arrays can be written by giving
each element a header with two square brackets, in this case `[[music]]`.  This file is equivalent to
writing the following (note that putting `music` in the bottom of the file would make it part of the
`[method]` header, which is not what we want):

> _yorkshire-s8-qps.toml (alternative):_
```toml
length = "QP"
base_calls = "near"

# This is new
music = [
    { run_lengths = [4, 5, 6, 7] },
    { patterns = ["*5678", "*8765", "5678*", "8765*", "*6578"], weight = 0.5 }
]

[method]
stage = 8
place_notation = "x38x14x58x16x12x38x14x78,12"
```

Both layouts are equally valid, and are up to personal preference.  The second form is arguably more
compact than the former, but could get unwieldy with large numbers of music definitions.

### Composition search

Our input file is now complete (yay - success!) and we can use Monument to generate some interesting
compositions.  Let's generate some compositions!

First, we need to open a command line in the same folder as `yorkshire-s8-qps.toml`.  Now, running
`monument yorkshire-s8-qps.toml` will ask Monument to find the answer for your question (expressed
in the form of the `.toml` file).

After hitting enter, Monument will hang momentarily and then print out the 30 best compositions,
sorted with the best last:

```text
$ monument yorkshire-s8-qps.toml
Using 12 threads.
  100.00% done:  64.5M nodes searched,   2.2M comps found
Completed in 127.02ms (508.1M nodes/s, 17.5M comps/s)
(len: 1280, score: 119  ) sHsWHBHsWBsHBMsWsH
(len: 1280, score: 119  ) sHBsMsHBsHsMsWsHBsHMsWsH
(len: 1280, score: 119  ) sHsMsMsWHBHBBMsWsH
(len: 1280, score: 119.5) sHBsMsHBsHsMsWHBHMsWsH
(len: 1280, score: 119.5) sHBsHsHsMBBMsWsH
(len: 1280, score: 119.5) sHBBsMWsHBsHHsMsH
(len: 1280, score: 119.5) sHsWHBHBsHsWsHBsHMsWsH
(len: 1280, score: 119.5) sHBsMBsMsWBsHMsH
(len: 1280, score: 119.5) sHBsHsHsMsHBBMsH
(len: 1280, score: 119.5) sHsWHBHBHWsHBsHMsWsH
(len: 1312, score: 122.5) sHBBsHBBHBHHsMsH
(len: 1280, score: 120  ) sHBsMBHBHsMsHMsWsH
(len: 1280, score: 120  ) sHBsMBHBHMHMsWsH
(len: 1280, score: 120  ) sHBsMBsWBsHMsWsH
(len: 1280, score: 120  ) sHsWHBHBHWHBHMsWsH
(len: 1280, score: 120  ) sHsWHBHBsHsWHBHMsWsH
(len: 1280, score: 120.5) sHBsMsHBHsWHBHsMsH
(len: 1280, score: 120.5) sHBBsHMsWBsHHsMsH
(len: 1280, score: 120.5) sHBsHBWsHBsHHsMsH
(len: 1312, score: 124  ) sHBsMBBBHBHsMsH
(len: 1280, score: 121.5) sHsWsHBsHsMsMsHBHBsH
(len: 1280, score: 121.5) sHBsMsHsMWBHBHsMsH
(len: 1280, score: 122  ) sHsWHBHBsWBsHMsWsH
(len: 1280, score: 122  ) sHsMsMsWHBHsHBHBsH
(len: 1280, score: 122.5) sHsWBsHsMBBMsWsH
(len: 1280, score: 123.5) sHsWHBHsMsMHBsHBsH
(len: 1280, score: 124  ) sHsMWsHBBsHBHHsMsH
(len: 1312, score: 127.5) sHBBsHBBsHBsHHsMsH
(len: 1280, score: 125  ) sHsWHHBsHBBsHMsWsH
(len: 1280, score: 127.5) sHsWHBHsMsMsHBHBsH
```

Wow!  Pretty fast.  Faster than I could do by hand, anyway.  It took 1/8th of a second to search
through all 2.2M possible compositions.

But there's something weird happening with the output.  Can you spot it?  I said that Monument will
produce the 30 best compositions and that they would be sorted.  But this clearly isn't true - the
last three compositions actually contain a step down in score:

```text
...
(len: 1312, score: 127.5) sHBBsHBBsHBsHHsMsH
(len: 1280, score: 125  ) sHsWHHBsHBBsHMsWsH
...
```

So what's going on?  Basically, Monument will (by default) sort compositions not by their absolute
score, but by their 'normalised' score - i.e. the absolute score divided by length.  This stops
Monument from biasing completely towards long compositions (generally resulting in a much saner
output) but causes the output rankings to often appear unsorted.  As with all defaults, this can be
overridden by adding `normalised_score = false` to the top section of the input file.
