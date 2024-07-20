# `require_atw`

**Defaults to `false`.**

If set to `true`, then Monument will filter out any compositions which are not fully all-the-work.

Note that **this is just a filter**.  Setting this to `true` will not make Monument generate a
higher proportion of all-the-work compositions -- it will just make Monument run its search for
longer because it will now not stop until it generates [`num_comps`](num_comps.md) _all-the-work_
compositions (rather than stopping after the first [`num_comps`](num_comps.md) compositions of any
description).
