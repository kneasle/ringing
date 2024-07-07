# `length`

**(This parameter is required)**

`length` is the only parameter which is always required (Monument also needs you to specify at
least one method, but methods can be added with either the `method` or `methods` parameters).

`length` determines the _inclusive_ range of lengths into which the compositions must fit.  This
can take the following forms:

```toml
length = "practice"  # equivalent to `{ min =    0, max =  300 }`
length = "QP"        # equivalent to `{ min = 1250, max = 1350 }`
length = "half peal" # equivalent to `{ min = 2500, max = 2600 }`
length = "peal"      # equivalent to `{ min = 5000, max = 5200 }`

length = { min = 600, max = 700 } # require length of 600-700 rows (inclusive)
length = 1729        # require an exact length
```

## Example 1: Quarter peals of Yorkshire Surprise Major

> **Input file:**
>
> ```toml
> length = "QP"
> method = "Yorkshire Surprise Major"
> ```
> 
> **Example output:**
>
> ```
>                            -- rest of output omitted for brevity --
> 1280 |  137.00 :   67 ( 30f  37b)    1f 11b    6f  0b    1f  5b     0 |  0.089219 | sHBBsHMsWBsHHsMsH
> 1344 |  138.00 :   72 ( 25f  47b)    1f 13b    6f  0b    0f  0b     0 |  0.087054 | sHsMsWsHWWHHsMsH
> 1280 |  138.00 :   73 ( 33f  40b)    0f 10b    6f  0b    3f  3b     0 |  0.087812 | sHsWsHBsHsMsMsHBHBsH
> 1312 |  138.00 :   73 ( 31f  42b)    0f 10b    4f  0b    2f  2b     0 |  0.088567 | sHBsMBBBHBHsMsH
> 1280 |  138.00 :   69 ( 31f  38b)    1f 11b    8f  0b    1f  5b     0 |  0.090000 | sHBsHBWsHBsHHsMsH
> 1312 |  140.00 :   68 ( 30f  38b)    2f 12b    7f  1b    1f  3b     0 |  0.088719 | sHBBsHBBHBHHsMsH
> 1280 |  140.00 :   74 ( 33f  41b)    0f 10b    6f  0b    3f  3b     0 |  0.090156 | sHsWHBHsMsMHBsHBsH
> 1280 |  141.00 :   73 ( 29f  44b)    2f 14b    7f  1b    0f  2b     0 |  0.090937 | sHsMWsHBBsHBHHsMsH
> 1280 |  142.00 :   73 ( 31f  42b)    2f 14b    7f  1b    0f  2b     0 |  0.091719 | sHsWHHBsHBBsHMsWsH
> 1280 |  144.00 :   77 ( 35f  42b)    0f 10b    6f  0b    3f  3b     0 |  0.093281 | sHsWHBHsMsMsHBHBsH
> 1312 |  145.00 :   72 ( 33f  39b)    2f 12b    7f  1b    1f  3b     0 |  0.091768 | sHBBsHBBsHBsHHsMsH
> -----|----------------------------------------------------------------|-----------|-----------
>  len |  music       4-bell runs      5678s     8765s     6578s    87s | avg score | calling
> 100 compositions generated in 277.66ms
> ```
>
> Note how a "Quarter Peal" is interpreted as 1250-1350 changes, so we are seeing a range from
> 1280 to 1344.  1250s and 1346s are also possible and will be generated, but are not shown here.



## Example 2: Peals of Bristol Surprise Royal

> **Input file:**
>
> ```toml
> length = "peal"
> method = "Bristol Surprise Royal"
> ```
> 
> **Example output:**
>
> ```
>                            -- rest of output omitted for brevity --
>  17 | 5160 |  919.00 :   542 ( 282f  260b)    27f  43b   10     2       0 |  0.168954 | MHsWsMMsMsWWMMWMWsMsWWMMsWsMWMWH
>  25 | 5200 |  920.00 :   547 ( 292f  255b)    31f  41b    7     4       0 |  0.168385 | MHWWHsMsWMsMsWWMWMWMHMWsWsMWH
>  26 | 5200 |  920.00 :   548 ( 288f  260b)    27f  41b   10     6       0 |  0.168385 | MHsWsMHMWsWsMHMWMWWMMsWsMWMWH
>  90 | 5200 |  924.00 :   530 ( 282f  248b)    32f  41b   13     2       0 |  0.169154 | MHsWsMMsMsWWMMWMsWsMWWWMWMMWH
>  63 | 5160 |  927.00 :   536 ( 291f  245b)    35f  39b    9     3       0 |  0.170504 | MHsWsMMsMsWMWHWMWMsMsWsHsMWMWMWM
>  30 | 5200 |  928.00 :   537 ( 291f  246b)    34f  39b   10     7       0 |  0.170308 | MHsWsMHMWWMWHHMWMWWMWMMWH
>  93 | 5200 |  929.00 :   533 ( 289f  244b)    37f  41b    5     3       0 |  0.170115 | MsMsWWMMWMsWMsWWMMWHWMWHHsWsM
> ----|------|--------------------------------------------------------------|-----------|-----------
>  #  |  len |  music         4-bell runs         lb5s     56s   65s    09s | avg score | calling
> 100 compositions generated in 1.59s
> ```
>
> Note how a "Peal" is interpreted as 5000-5200 changes.
>
> Also note how Monument sorts the final composition list by absolute music score, meaning that the
> compositions which appear at the end are generally longer, as they contain more rows and can
> therefore accrue more music.



## Example 3: Excluding compositions over 5100

> This is the same as [Example 2](#example-2-peals-of-bristol-surprise-royal) (peals of Bristol Royal),
> except that in this case we want to restrict the search to only compositions under 5100 rows (since
> they are more likely to be rung).
>
> **Input file:**
>
> ```toml
> length = { min = 5000, max = 5100 }
> method = "Bristol Surprise Royal"
> ```
> 
> **Example output:**
>
> ```
>                            -- rest of output omitted for brevity --
>  75 | 5002 |  905.00 :   537 ( 286f  251b)    34f  41b    4     4       0 |  0.173031 | MHWWMMsWsHsWsMHWWsMsWMWWMsW>
>   5 | 5000 |  905.00 :   549 ( 286f  263b)    28f  39b    5     9       0 |  0.173360 | MMsWsMHWMsMsWWMHsWsMMsMsWWM
>  17 | 5042 |  906.00 :   534 ( 271f  263b)    27f  41b   11     7       0 |  0.171817 | MHWWHHsHsMsHsMWsMsWsHsMsMsWWsW>
>   1 | 5040 |  906.00 :   533 ( 275f  258b)    28f  39b   16     2       0 |  0.172738 | HWsMsWWMsMsWWsWsMHWWHHMH
>   9 | 5000 |  906.00 :   510 ( 269f  241b)    35f  41b    7     6       0 |  0.173560 | MsMsWWMMHWMWHsWsMMMsHsMsWsM
>   2 | 5002 |  907.00 :   531 ( 280f  251b)    30f  41b    9     6       0 |  0.173631 | MsMsWWMWMWMHMWsWsMHWMHWsW>
>   6 | 5040 |  908.00 :   546 ( 284f  262b)    31f  39b    5     7       0 |  0.173135 | MHWMWHsWMsMMHsWsMMsMsWWM
>   4 | 5040 |  910.00 :   534 ( 277f  257b)    31f  39b   12     2       0 |  0.173333 | HWsMsWWMsMsWWsWsMHWWHHsHsM
> ----|------|--------------------------------------------------------------|-----------|-----------
>  #  |  len |  music         4-bell runs         lb5s     56s   65s    09s | avg score | calling
> 100 compositions generated in 1.70s
> ```
>
> Note how no compositions over 5100 were generated (unlike in
> [Example 2](#example-2-peals-of-bristol-surprise-royal))
