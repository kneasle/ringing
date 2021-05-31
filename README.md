# Monument

A experimental, optimal composing engine with a taste for music.

## Current Features
- Optimal and complete: i.e. if the optimal composition exists, Monument will find it eventually.
  It never gets stuck in an infinite loop

## Intended Features
- Course head/coursing order patters (e.g. bias towards courses with handbell pairs coursing)
- Adding weights to calling patterns (e.g. to bias towards sparser callings, or to prevent blocks of
  5 in Bristol Major)
- Allowing specific patterns of split-tenor courses (e.g. `VF`, `FI`, `IV`)
- Support for snap starts and finishes (and all combinations thereof).  This is remarkably difficult
  to implement without breaking Monument's design goals.

## Goals of Monument

- **Optimality**: When given a specification and asked for `n` compositions, Monument will generate
  the `n` best compositions according to the specification.
- **Completeness**: If the specification is satisfiable, Monument will always find it.  It might take
  an infeasibly long time, but Monument will discover it eventually.
- Monument's time and memory requirements should be as **independent** of the search space size as
  possible.  Searches like fully split tenors Maximus should be possible.

## Non-goals

- Spliced.  Generating good spliced is a completely different task to single method composing.
  Monument's IDA\* engine and heuristics could possibly be used, but that's another project.
