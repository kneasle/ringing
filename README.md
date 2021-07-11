# Monument

A general-purpose, music-oriented composing engine for [Change
Ringing](https://en.wikipedia.org/wiki/Change_ringing).

A tutorial can be found [here](tutorial.md), but the tutorial is just as W.I.P. as Monument itself.

## Features

- **General purpose**: The core composing engine makes no assumptions about the compositions that
  it's generating - it just takes as input a set of blocks of rows and information about how they
  can be joined together (see the `Layout` type).  Therefore Monument can, in theory, handle
  arbitrary searches, e.g. spliced.  However, as of yet Monument has only been profiled on single
  method searches and is likely missing optimisation opportunities for other searches.
- **Optimal & Complete**: Given a definition of 'good' and some positive integer `n`, Monument will
  not terminate until it has generated the `n` _best_ compositions according to that definition
  (which satisfy the other constraints, e.g.  length).  It might take a long time to completely rule
  out the possibility of better compositions, so Monument tries to reorder the search as much as
  possible to generate good compositions quickly (so even partial runs will produce good results).
- **Human-friendly**: The input format should be as human friendly as possible, with shortcuts to do
  common tasks (e.g. asking for 4-, 5-, 6- and 7-bell runs) and sane defaults (e.g. generate
  standard calling positions by default).  Monument uses [TOML](https://toml.io/en/) for its input
  format, an existing configuration file format with designed with humans as a priority.
- **Fast**: Monument should be as fast as possible without impacting reliability or correctness.
  Note here that the only thing that matters is how quickly Monument produces the best compositions
  \- it's absolutely fine to reduce the number of nodes per second in order to do pruning/reordering
  which speeds up the overall search.  Most of the examples in [the `examples/` directory](examples)
  are used as benchmarks.
- **Multi-threaded**: Monument's search algorithm will take advantage of as many CPU cores as it can
  \- twice the CPU cores, half the search time.  This is not linear forever, but the non-linearity
  will only become noticeable if you have at least thousands of CPU cores (which seems unachievable).

## Intended Features

- Course head/coursing order patters (e.g. bias towards courses with handbell pairs coursing)
- Adding weights to calling patterns (e.g. to bias towards sparser callings, or to prevent blocks of
  5 in Bristol Major)
- Allowing specific patterns of split-tenor courses (e.g. `VF`, `FI`, `IV`)
- Require specific blocks (to run linkage search)

## Intentional Non-Features

- Rotational search: This is a search technique employed by [SMC](https://github.com/GACJ/smc)
  where you only search one rotation of every possible composition (e.g. only generate one of
  `WWWHHH`, `HWWWHH`, `HHWWWH`) and rotate them afterwards.  This is clearly an advantageous way to
  generate every possible composition - these rotations are either all true or all false and there's
  no point proving them all separately - so why not use it?  The answer is that rotational search
  has a few huge disadvantages:
  1. The benefit of rotational search only really becomes apparent when generating _every possible_
     composition.  But Monument is only trying to find the **best** compositions, meaning that it
     can optimise the search by removing large swathes of the search space.
  2. Rotational search can't be easily parallelised, immediately missing out on a guaranteed
     speedup of at least 8x on modern machines (speedup which always apply to any search).
  3. Additionally, rotational search always outputs the compositions in a specific, deterministic
     order.  Therefore, if the best composition exists near the end of a rotational search there is
     no way for the algorithm to produce it faster - you always have to wait for the entire search
     to finish before the results can be proven optimal.  Again, Monument hopes to optimise this by
     strategically reordering the search space so that good compositions appear quickly.
  4. Rotational search only speeds up the search by the symmetry factor of the search space.  At
     best, this factor is around 30 (the number of courses in a peal of Surprise Major, plus a bit
     to factor in short courses).  But often times the search space has no overall symmetry - for
     example, if you have required blocks, want to include specific courses (e.g. `1xxx8765` into an
     otherwise tenors together comp), want to include snap finishes or want to generate multi-part
     compositions then rotational search gives no benefit.  Finally, the searches which benefit most
     from rotational search (i.e. fully split tenors searches) are often intractible anyway, since
     the search space is exponential in the length whereas rotational search's improvement is at
     best linear.
