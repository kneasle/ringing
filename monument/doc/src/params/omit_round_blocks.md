# `omit_round_blocks`

**_(Unreleased)_**

**Defaults to `false`.**

If a custom [`calling`](calling.md) is given, setting this to `true` will allow Monument to skip round
blocks in the input calling.  For example, if this is set to `true` then a calling of
"WWWHHH" would generate compositions using "" (i.e. the plain course), "WWW", "HHH" and "WWWHHH".
