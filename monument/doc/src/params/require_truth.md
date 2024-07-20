# `require_truth`

**Defaults to `true`.**

If set to `true` (the default value), Monument will not allow falseness and only emit true
compositions.  Conversely, if explicitly set to `false`, this will generate false compositions.


## Example 1: True Practice Touches

> **Input file:**
>
> ```toml
> length = "practice"
> methods = ["Yorkshire Surprise Major", "Bristol Surprise Major"]
> method_count = { min = 64, max = 300 }
> require_truth = true # Note: this could be omitted, as `true` is the default value
> 
> base_music = "none" # Hide music to make output cleaner
> ```
>
> **Example output:**
> 
> ```
>              -- rest of output omitted for brevity --
>   7 | 288 : 160 128 |    0.00 | -0.025000 | BB[M]BY[H]YYY[M]BY[H]
>   8 | 288 : 160 128 |    0.00 | -0.025000 | BY[W]BB[H]BY[W]YYY[H]
>   6 | 288 : 160 128 |    0.00 | -0.025000 | BY[W]YYY[H]BY[W]BB[H]
>   3 | 288 : 160 128 |    0.00 | -0.025000 | YYY[M]BY[H]BB[M]BY[H]
>   2 | 290 : 160 130 |    0.00 | -0.020345 | YYYY[W]BBB[sM]B[M]YB>
>   4 | 290 : 160 130 |    0.00 | -0.020345 | YYYY[W]BB[H]B[sH]BYB>
>   1 | 288 : 224  64 |    0.00 | -0.018750 | YYYYYYY[H]B[H]B[H]
>   5 | 288 : 224  64 |    0.00 | -0.018750 | YYYY[W]B[W]B[W]YYY
>  25 | 288 : 224  64 |    0.00 | -0.018750 | YYY[M]B[M]B[M]YYYY
> ----|---------------|---------|-----------|-----------
>  #  | len    Y   B  |  music  | avg score | calling
> 100 compositions generated in 264.34ms
> ```
>
> Here, we are making some practice night touches of Yorkshire and Bristol spliced.  Splicing from Yorkshire to
> Bristol without a call is immediately false, so notice how Monument is careful to only ever splice
> from Bristol to Yorkshire (and not back):


## Example 2: False Practice Touches

> **Input file:**
>
> ```toml
> length = "practice"
> methods = ["Yorkshire Surprise Major", "Bristol Surprise Major"]
> method_count = { min = 64, max = 300 }
> require_truth = false
> 
> base_music = "none" # Hide music to make output cleaner
> ```
>
> **Example output:**
> 
> ```
>              -- rest of output omitted for brevity --
>  47 | 256 : 160  96 |    0.00 |  0.000000 | YYBYBYBY
>  46 | 256 : 160  96 |    0.00 |  0.000000 | YYBYBYYB
>   6 | 288 :  96 192 |    0.00 |  0.000000 | YYYBBBBBB
>   4 | 256 : 160  96 |    0.00 |  0.000000 | YYYBBBYY
>   7 | 256 : 160  96 |    0.00 |  0.000000 | YYYBBYBY
>   5 | 256 : 160  96 |    0.00 |  0.000000 | YYYBBYYB
>   9 | 256 : 160  96 |    0.00 |  0.000000 | YYYYYBBB
> ----|---------------|---------|-----------|-----------
>  #  | len    Y   B  |  music  | avg score | calling
> 100 compositions generated in 288.26ms
> ```
>
> Note how Monument is generating blatantly false compositions here - splicing from Yorkshire to
> Bristol is false, and furthermore Monument is generating a 8-lead long course when there are only
> 7 lead ends available.
