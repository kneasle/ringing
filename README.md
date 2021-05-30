# Zimmer

A experimental, optimal composing engine with a taste for music.

## Goals of Zimmer

- **Optimality**: When given a specification and asked for `n` compositions, Zimmer will generate
  the `n` best compositions according to the specification.
- **Completeness**: If the specification is satisfiable, Zimmer will always find it.  It might take
  an infeasibly long time, but Zimmer will discover it eventually.
- Zimmer's time and memory requirements should be as **independent** of the search space size as
  possible.  Searches like fully split tenors Maximus should be possible.

## Non-goals

- Spliced.  Generating useful spliced is a completely different task to single method composing.
