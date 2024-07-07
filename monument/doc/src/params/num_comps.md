# `num_comps`

**Defaults to `100`.**

The number of compositions that Monument will emit before finishing the search.

Monument tends to generate compositions in decreasing order of goodness, so the _first_ `num_comps`
compositions found will correspond roughly to the _best_ `num_comps` compositions.  Note that this
based on the _average score per row_ (i.e. total score divided by length) - which prevents Monument
from always generating long compositions because of the extra score from the extra rows.

When printing the final summary, Monument sorts the composition by total music score.

## Example 1: Emit only the first (and likely best) 5 compositions

> **Input file:**
>
> ```toml
> length = "QP"
> method = "Yorkshire Surprise Major"
> num_comps = 5
> ```
>
> **Example output:**
> 
> ```
>                            -- rest of output omitted for brevity --
>
>
> SEARCH COMPLETE!
> 
> 
> 
> # |  len |  music       4-bell runs      5678s     8765s     6578s    87s | avg score | calling
> --|------|----------------------------------------------------------------|-----------|-----------
> 4 | 1280 |  132.00 :   66 ( 30f  36b)    1f 11b    8f  0b    1f  5b     0 |  0.087500 | sHsWHsHBMBHBsH
> 5 | 1280 |  134.00 :   75 ( 29f  46b)    0f 12b    4f  0b    1f  1b     0 |  0.088281 | sHsWBsHsMBBMsWsH
> 3 | 1280 |  138.00 :   74 ( 33f  41b)    0f 10b    6f  0b    3f  3b     0 |  0.088594 | sHsWHBHsMsMHBsHBsH
> 2 | 1280 |  140.00 :   73 ( 31f  42b)    2f 14b    7f  1b    0f  2b     0 |  0.090156 | sHsWHHBsHBBsHMsWsH
> 1 | 1280 |  142.00 :   77 ( 35f  42b)    0f 10b    6f  0b    3f  3b     0 |  0.091719 | sHsWHBHsMsMsHBHBsH
> --|------|----------------------------------------------------------------|-----------|-----------
> # |  len |  music       4-bell runs      5678s     8765s     6578s    87s | avg score | calling
> 5 compositions generated in 272.91ms
> ```
