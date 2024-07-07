# `part_head`

**Defaults to `""` (i.e. rounds)**

A row which generates the part head structure for this composition.  Monument will automatically
add extra tenors to the given row, so `"1342"` on Major is equivalent to `"13425678"`, and the
empty string (`""`) is equivalent to rounds (i.e. a 1-part composition).

Monument repeatedly applies this row to get the possible parts, for example on Major:

- `part_head = "1342"` will generate three parts, starting at `12345678`, `13425678` and `14235678`.
  This is the classic 3-part, keeping `5`, `6`, `7` and `8` fixed while rotating `2`, `3` and `4`.
  See [Example 1](#example-1-3-part-rotating-234).

- `part_head = "18234567"` will generate seven parts, starting at `12345678`, `18234567`, `17823456`,
  `16782345`, `15678234`, `14567823` and `13456782`.  This would be a cyclic 7-part with the treble
  fixed.

- `part_head = "81234567"` will generate eight parts, starting at `12345678`, `81234567`, `78123456`,
  `67812345`, `56781234`, `45678123`, `34567812` and `23456781`.  This would be a variable-treble
  cyclic 8-part.  See [Example 2](#example-2-cyclic-8-part).

However, **Monument allows the parts to be rung in any order, so long as all the parts get rung**.
Therefore, `part_head = "81234567"` will also generate compositions where the first part ends in
`67812345` (since this also generates all eight parts).  However, it _will not_ generate compositions
where the first part ends in `56781234`, since that would only generate two of the eight required
parts.



## Example 1: 3-part rotating 2,3,4

> **Input file:**
>
> ```toml
> length = "QP"
> method = "Plain Bob Major"
> part_head = "1342"
> ```
> 
> **Example output:**
>
> ```
>                            -- rest of output omitted for brevity --
>   1 | 1344 | 1423 |  166.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.101190 | sMMsMMH
>   7 | 1344 | 1423 |  168.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.090179 | sHsMsHsWsWMsH
>   8 | 1344 | 1423 |  168.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.090179 | sHsWsWsMsHMsH
>  23 | 1344 | 1342 |  168.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.090179 | sMsHMsHsWsWsH
>  74 | 1344 | 1342 |  168.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.090179 | sMsWsHsWMsHsH
>   4 | 1344 | 1423 |  168.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.092411 | sHMsHWsWMsH
>  47 | 1344 | 1342 |  168.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.092411 | sMsWHWMsHsH
>   3 | 1344 | 1423 |  170.00 :   82 ( 41f  41b)    0f 24b   24f  0b    0f  0b     0 |  0.093899 | sHsMHWsWMsH
>  32 | 1344 | 1423 |  170.00 :   78 ( 39f  39b)    0f 24b   24f  0b    0f  0b     0 |  0.097917 | MsMHsMsHsH
>  29 | 1344 | 1423 |  172.00 :   80 ( 40f  40b)    0f 24b   24f  0b    0f  0b     0 |  0.099405 | MMsHsMsHsH
> ----|------|------|----------------------------------------------------------------|-----------|-----------
>  #  |  len |  PH  |  music       4-bell runs      5678s     8765s     6578s    87s | avg score | calling
> 100 compositions generated in 288.38ms
> ```
>
> Note how, even though we gave `1342` as the part head, `1423` is sometimes being used.  This is OK
> because it will still generate the same parts, only in a different order.

## Example 2: Cyclic 8-part

> **Input file:**
>
> ```toml
> length = "practice"
> method = "Original Major"
> part_head = "81234567"
> ```
> 
> **Example output:**
>
> ```
>                            -- rest of output omitted for brevity --
>  60 | 272 | 23456781 |  111.20 :   60 ( 25f  35b)    1f  5b    4f  2b    0f  0b     4 | -0.073529 | #OOOOO[s]OOO[s]OO[s]O[-]OOO[-]O[s]O[-]O[-]
>  59 | 272 | 23456781 |  111.20 :   60 ( 25f  35b)    2f  5b    3f  2b    0f  0b     4 | -0.073529 | #OOOOO[s]OO[-]O[s]OO[s]OOOO[-]O[-]O[-]O[s]
>  58 | 272 | 23456781 |  111.20 :   60 ( 25f  35b)    1f  5b    4f  2b    0f  0b     4 | -0.073529 | #OOOOO[s]OO[-]O[s]OO[s]OOOO[-]O[s]O[-]O[-]
>  30 | 288 | 67812345 |  113.30 :   60 ( 25f  35b)    2f  6b    3f  1b    0f  0b     1 | -0.048264 | #OOOOOOO[s]O[-]OOO[s]OOO[-]O[-]O[s]O[-]O[-]
>  29 | 288 | 67812345 |  113.30 :   60 ( 25f  35b)    2f  6b    3f  1b    0f  0b     1 | -0.048264 | #OOOOOO[-]O[s]OOOO[s]OOO[-]O[-]O[s]O[-]O[-]
>  13 | 240 | 81234567 |  122.00 :   70 ( 30f  40b)    4f  2b    2f  6b    0f  0b     0 |  0.055000 | #O[s]OOOOO[-]O[-]O[s]O[-]O[-]OOO[-]OO
>  53 | 272 | 81234567 |  126.00 :   70 ( 30f  40b)    2f  6b    4f  2b    0f  0b     0 | -0.072059 | #O[s]OOOOO[s]OOO[-]OO[-]O[s]OO[-]O[s]O[-]O[-]
>  70 | 272 | 81234567 |  126.00 :   70 ( 30f  40b)    4f  2b    2f  6b    0f  0b     0 | -0.072059 | #O[s]OOOOO[s]O[-]O[-]O[s]O[-]OO[s]O[-]OO[-]OO
>  98 | 272 | 81234567 |  134.00 :   80 ( 30f  50b)    4f  4b    2f  6b    0f  0b     0 | -0.013235 | #O[-]O[s]O[-]OOOOO[-]O[-]O[s]O[-]O[-]OOO[-]OO
>  72 | 272 | 81234567 |  138.00 :   80 ( 30f  50b)    4f  2b    2f  8b    0f  0b     0 |  0.001471 | #O[s]OOOOO[-]O[-]O[-]O[s]O[-]O[-]O[-]OOO[-]OO
> ----|-----|----------|----------------------------------------------------------------|-----------|-----------
>  #  | len |    PH    |  music       4-bell runs      5678s     8765s     6578s    87s | avg score | calling
> 100 compositions generated in 572.06ms
> ```
>
> Note how, even though we gave `18234567` as the part head, `23456781` and `67812345` are sometimes being used
> (as is `45678123`, though it isn't shown here).  However, the other part heads (`78123456`, `56781234` and
> `34567812`) are never used, because they would not generate all the parts.
