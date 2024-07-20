# `atw_weight`

Weight applied to a composition that is 'all the work' (i.e. one where every bell rings every part
of every place bell of every method).  The score generated is continuous - a 'half-the-work'
composition will be given an extra score of `atw_weight * 0.5`.

Setting this to any value (even `0.0`) will cause the atw-ness of compositions to be displayed.

**NOTE:** Monument's current search algorithm finds it quite challenging to actually find fully atw
compositions.  The algorithm is designed to generate 'all-rounder' compositions, and therefore
struggles with 'all-or-nothing' metrics like atw - it will likely generate a lot if 90+% atw
composition.
