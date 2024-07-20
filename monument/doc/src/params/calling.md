# `calling`

**_(Unreleased)_**

Specifies a call string which all compositions must follow.  If unspecified, Monument can use any calling.

This is useful when composing spliced, 
to make Monument fit methods around a known good calling.  This is written as a sequence of calling
positions from the perspective of the [`calling_bell`](calling_bell.md) **in the first part**.

Any whitespace characters in the string will be ignored.

### Using cyclic callings from CompLib

When using callings which affect the tenor, Monument's default behaviour is different to that of
[CompLib](complib.org), and this can cause confusing errors as a seemingly correct calling will just not
work.  By default, CompLib specifies multi-part callings from the tenor **in the last part**, with
a note saying which bell that corresponds to in the first part -- whereas Monument uses the tenor's
calling positions **in the first part**.

You can use a calling from CompLib by setting the
[`calling_bell`](calling_bell.md) to the one displayed on CompLib.  For example, the calling from
[this composition](https://complib.org/composition/46787) can be replicated with:

```toml
calling = "IFV IWM"
calling_bell = 7    # From CompLib: "(7th observation in the first part)"
```

---

## Example 1: 1282 of Cambridge and Yorkshire Royal

> The following will make Monument fit Cambridge and Yorkshire Surprise Royal to the
> classic Quarter Peal composition of [`HHsWsHsW`](https://complib.org/composition/12026):
>
> **Input file:**
>
> ```toml
> length = "QP"
> methods = [
>     "Cambridge Surprise Royal",
>     "Yorkshire Surprise Royal",
> ]
> calling = "HHsWsHsW"
> 
> base_music = "none" # Simplify output
> splice_weight = 1   # Encourage splicing, making Monument try to splice every lead
> ```
>
> **Example output:**
>
> ```
>  39 | 1282 : 602 680 |    0.00 |  0.015991 | YCYCYCYCY[H]CYCYCYYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>  13 | 1282 : 602 680 |    0.00 |  0.015991 | YCYCYCYCY[H]CYCYYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>  56 | 1282 : 602 680 |    0.00 |  0.015991 | YCYCYCYCY[H]CYYCYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>  16 | 1282 : 602 680 |    0.00 |  0.015991 | YCYCYCYCY[H]YCYCYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>  23 | 1282 : 602 680 |    0.00 |  0.015991 | YCYCYCYYC[H]YCYCYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>  25 | 1282 : 602 680 |    0.00 |  0.015991 | YCYCYYCYC[H]YCYCYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>  27 | 1282 : 602 680 |    0.00 |  0.015991 | YCYYCYCYC[H]YCYCYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>   1 | 1282 : 642 640 |    0.00 |  0.016771 | CYCYCYCYC[H]YCYCYCYCY[H]CYCYC[sW]YCYC[sH]YCYCY[sW]C>
>   2 | 1282 : 640 642 |    0.00 |  0.016771 | YCYCYCYCY[H]CYCYCYCYC[H]YCYCY[sW]CYCY[sH]CYCYC[sW]Y>
> ----|----------------|---------|-----------|-----------
>  #  |  len    C   Y  |  music  | avg score | calling
> 100 compositions generated in 373.46ms
> ```
>
> Note how all the compositions use the same calling, but different method sequences.

## Example 2: Cyclic spliced

> **TODO**
