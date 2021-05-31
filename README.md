# Monument

A experimental, optimal composing engine with a taste for music.

## Goals of Monument

- **Optimality**: When given a specification and asked for `n` compositions, Monument will generate
  the `n` best compositions according to the specification.
- **Completeness**: If the specification is satisfiable, Monument will always find it.  It might take
  an infeasibly long time, but Monument will discover it eventually.
- Monument's time and memory requirements should be as **independent** of the search space size as
  possible.  Searches like fully split tenors Maximus should be possible.

## Non-goals

- Spliced.  Generating good spliced compositions is a completely different task to single method
  composing.  Monument's IDA\* engine could possibly be used, but that's another project.
