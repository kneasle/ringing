# Monument

An experimental, music oriented composing engine.

In general, there are two approaches to generating compositions.  The first is exhaustive search,
where the aim is to generate every possible composition as efficiently as possible.  This is the
approach taken by [SMC](https://github.com/GACJ/smc) to great effect.  Exhaustive searches can be
optimised extremely well (SMC's rotational search is a great example of this), and work well with
relatively false methods because the falseness drastically reduces the search size.

However, composers are generally not interested in every possible composition (and if you are, SMC
is your friend).  Instead, we want _good_ compositions and if we are using an engine we want the
engine to get to the good stuff as soon as possible.  In most cases, this does not require generating every
possible composition.

This brings us to the second approach - A\* search and its variants (the strategy employed by
Monument).  In A\* search, the engine predicts an upper bound for the best music score achievable
from any given point in the search.  The point of A\* is to use this additional information to
re-order which compositions are searched - i.e. to search the good stuff first.  Note that this is
simply a re-ordering - A\* search will still explore every possible composition but we will get the
results we want more quickly (despite the slower search speed).

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
