# Introduction

## What is Monument?

Monument is a fast and human-friendly composing engine.

The goal is to **get you compositions that you'll enjoy ringing, as quickly and
easily as possible.**

### Monument is _fast_

Like, seriously fast.

Monument is often **orders of magnitude faster** than any other composing program I'm aware of.

The trade-off is that Monument **does not guarantee perfection**: Monument aims to generate _very
good_ all-rounder compositions in massively less time than it would take to run an 'perfect'
exhaustive search.  The logic here is that you almost certainly don't care about absolute perfection;
you want something you will enjoy ringing, and you don't want to have to wait ages to get it.

### Monument does the 'right thing' by default

By default, Monument always tries to do 'the right thing'.  By default, Monument does the following:
- Add fourths-place ('near') bobs and singles
- Restrict to strictly tenors-together
- Add a whole bunch of common music definitions (n-bell runs, 56s, 65s, 87s at back, etc.).
- Picks sane method balance requirements

Among other things, this means that simple searches can be done with extremely short input files.
Want a peal of Bristol Royal?  Two lines:

> Input file:
> ```toml
> length = "peal"
> method = "Bristol Surprise Royal"
> ```
> 
> Example output:
> ```
>                            -- rest of output omitted for brevity --
> 5122 |  385.00 :   530 ( 269f  261b)     62    17     2 |  0.068313 | MHsWsMMsMsWsMsHsWsMMHWWsWW>
> 5042 |  385.00 :   552 ( 278f  274b)     66     8     2 |  0.068485 | MsMsWsWMsWsHsMMsMsWMHsWsMMHWsW>
> 5082 |  385.00 :   520 ( 273f  247b)     60    16     6 |  0.068497 | MHsWsMsHsMWMHsWsMsMWsWHWHsW>
> 5000 |  385.00 :   493 ( 255f  238b)     63    20     4 |  0.069360 | WWsHsMsMsWWWHsMsWsHsMWWHHMH
> 5042 |  386.00 :   533 ( 265f  268b)     61    14     4 |  0.068881 | MsMsWMsWsHMsMsWMHsWsMMHWWsWW>
> 5000 |  386.00 :   499 ( 263f  236b)     63    16     4 |  0.069760 | WWsHsMMWHWHsMsWsHsMWWHHMH
> 5082 |  388.00 :   543 ( 277f  266b)     62    13     2 |  0.069087 | MHsWsMMHWsMsWsHsMsMWsWHWHsW>
> 5082 |  388.00 :   534 ( 278f  256b)     60    14     6 |  0.069284 | MHsWsMHMWsMWsWHHsWsMMHWsW>
> 5122 |  390.00 :   542 ( 282f  260b)     60    14     6 |  0.069680 | MHsWsMHMWMHsWsMMHWWsWW>
> 5122 |  393.00 :   543 ( 279f  264b)     67    11     6 |  0.070266 | MHWMWHsWMsMMHsWsMMHWsW>
> 5082 |  396.00 :   542 ( 275f  267b)     62    14     2 |  0.070661 | MHsWsMMsMsWsMWsWHHsWsMMHWsW>
> 5082 |  397.00 :   528 ( 272f  256b)     60    16     6 |  0.071055 | MHsWsMsHsMWMHWWHHsHsMsMWW>
> 5122 |  398.00 :   550 ( 279f  271b)     62    14     2 |  0.071046 | MHsWsMMsMsWMHsWsMMHWWsWW>
> -----|--------------------------------------------------|-----------|-----------
>  len |  music         4-bell runs      lb5s   56s   65s | avg score | calling
> 100 compositions generated in 818.91ms
> ```

Pretty sweet.

Also, see what I mean about speed?  That search took **0.818 seconds**!  I don't know
about you, but I struggle to compose anything very exciting in under one second - let alone make
one hundred nice compositions.

And these compositions are actually good!  For example, you can check out the second to top
composition [on Composition Library](https://complib.org/composition/104684?accessKey=20e68fb231540ee4c1ecb18678bb1e8b773537c8).
You can definitely get improve the results with some tuning, but these are pretty darn good given
how little effort it took.
