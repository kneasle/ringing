# Monument

A fast and flexible composing engine for
[method ringing](https://en.wikipedia.org/wiki/Method_ringing).

## Goals

Goals are roughly ordered with most important first.

- **Correct**: Monument should always produce compositions which satisfy your requirements, without
  crashes or unexpected behaviour.
- **Fast**: Monument should complete most searches in the order of seconds.  Monument achieves this
  at the cost of _optimality_: Monument can't guarantee that its compositions are the _best_
  (according to your definition of 'good') but instead tries to generate very good compositions
  as quickly as it can.
- **General**: Monument's core search routines are completely generic and have no understanding of
  exactly what they're optimising.  There are custom functions for doing common things easily (e.g.
  generating graphs for spliced) but Monument is just a Rust library so searches can be constructed
  programmatically if needed.
- **Flexible**: Monument also allows a large amount of tuning to encourage it to generate only the
  compositions you want.  It also provides parameters to tune the search algorithms to improve
  performance.
- **Human Friendly**: Common searches should have a simple, human-oriented user interface.
  For more specific searches, writing code will probably be required.

## Composing is Hard

Before describing Monument in more detail, it's worth pointing out that generating compositions is,
in general, **really hard** (NP hard, in fact).  There will always be queries which are simply too
hard for Monument (or any other engine) to complete, so any promise of speed is a best-effort not a
guarantee.  Currently, Monument is very good at wide search spaces with little falseness and a large
variety in music potential.  As an example, Monument will work better with e.g. Bristol, which has
very little falseness and a big gap in music potential between courses, versus e.g. Lincolnshire,
which has lots of falseness and all courses have roughly the same (bad) music output.

## Example

The following TOML file (found [here](examples/include-8765-courses.toml)) will run a search for
QPs of Yorkshire and Lessness spliced, using entirely tenors-together courses but allowing
`15678xxx` for the 8765s.  4ths place (near) calls are used by default:

```toml
length = "QP"
methods = [
    "Yorkshire Surprise Major",
    { title = "Lessness Surprise Major", shorthand = "E" },
]
course_heads = ["1xxxxx78", "15678xxx"]

[[music]]
run_lengths = [4, 5, 6, 7, 8]

[[music]]
patterns = ["*6578", "*5678", "*8765", "5678*", "8765*"]
```

This search takes about 30 seconds to generate 30 compositions, but the first composition was found
in ~1s (and ended up being 4th best overall).  The output will be something like:
```text
                -- snip --
len: 1280, ms: [640, 640], score: 175.20, avg: 0.136875, str: EYYYY[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]EYE[H]EYE[H]YY[B]E[sV]YEYYE[M]EYYYE[sF]EYY[sW]E[sH]YE[W]E[H]
len: 1280, ms: [640, 640], score: 175.20, avg: 0.136875, str: EYYYY[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]EYE[H]EYE[H]YY[B]E[sV]EYYYE[M]EYYEY[sF]EYY[sW]E[sH]EY[W]E[H]
len: 1280, ms: [640, 640], score: 175.20, avg: 0.136875, str: EYYYY[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]EYE[H]EYE[H]YY[B]E[sV]EYYYE[M]EYYYE[sF]EYY[sW]E[sH]EY[W]E[H]
len: 1280, ms: [640, 640], score: 175.40, avg: 0.137031, str: YYYYYYY[H]EYE[H]E[sM]Y[sW]EE[M]YE[H]EYE[H]EEE[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YEE[M]Y[sW]E[H]
len: 1280, ms: [640, 640], score: 175.80, avg: 0.137344, str: EYYYY[H]EYE[H]E[sM]Y[W]EE[sM]Y[W]E[H]EYE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YEE[M]Y[sW]E[H]
len: 1280, ms: [640, 640], score: 175.80, avg: 0.137344, str: EYE[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]YYYYE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YE[sH]E[M]Y[W]E[H]
len: 1344, ms: [672, 672], score: 185.00, avg: 0.137649, str: EYE[H]EYE[H]E[sM]Y[sW]EE[M]YE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EYY[W]YYE[B]EYY[M]Y[sW]EE[M]Y[sW]E[H]
len: 1280, ms: [608, 672], score: 176.20, avg: 0.137656, str: EYE[H]EYE[H]EY[W]E[H]YEE[H]YYEY[sT]YYE[M]EYE[M]YY[sF]E[B]EEE[H]EYYEY[B]EY[B]YEY[M]Y[W]E[H]
len: 1280, ms: [608, 672], score: 176.20, avg: 0.137656, str: EYE[H]EYE[H]EY[W]E[H]EYE[H]YYEY[sT]YYE[M]EYE[M]YY[sF]E[B]EEE[H]EYYEY[B]EY[B]YEY[M]Y[W]E[H]
len: 1280, ms: [640, 640], score: 176.60, avg: 0.137969, str: YYYYE[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]EYE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YEE[M]Y[sW]E[H]
len: 1344, ms: [672, 672], score: 185.80, avg: 0.138244, str: EYE[H]EYE[H]E[sM]Y[sW]EE[M]YE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EYY[W]YYE[B]EYY[sM]Y[sW]EE[M]YE[H]
len: 1280, ms: [640, 640], score: 177.60, avg: 0.138750, str: EYE[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]YYYYE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YEE[M]Y[sW]E[H]
len: 1280, ms: [640, 640], score: 177.80, avg: 0.138906, str: EYYYY[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]EYE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YE[sH]E[M]Y[W]E[H]
len: 1280, ms: [640, 640], score: 179.60, avg: 0.140312, str: EYYYY[H]EYE[H]EY[W]EE[sM]Y[sW]E[H]EYE[H]EYE[H]YY[B]E[sV]YY[M]EYE[M]EYE[M]YY[sF]EY[sM]YEE[M]Y[sW]E[H]
```
The columns `len`, `score` are self-explanatory.  `str` is a human-readable composition string
(which [`to-complib.py`](to-complib.py) can convert) and `ms` shows the number of rows of each
method. `avg` is the average music score generated by each row (i.e. `score`/`len`).  Monument
always ranks compositions by this average score to prevent the engine from overly favouring long
compositions just because they get higher scores.

More examples can be found in the [`examples/` directory](examples).
