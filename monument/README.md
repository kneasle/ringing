# Monument

A fast, flexible and human-friendly composing engine.  Monument is currently in an **alpha** state of
readiness (see the [known issues](#known-issues)).

_A guide to Monument can be found [here](cli/guide.md)._

## Goals

The ultimate goal of Monument is to **get you compositions that you want to ring, as quickly and
easily as possible**.  Monument should also be correct and performant, but this should be a goal of
all software.

Note that this doesn't necessarily mean generating the perfect composition, or generating _every
possible_ composition.  Monument guarantees neither of these but in return, Monument is **orders of
magnitude faster** than any other engine I'm aware of (if anyone knows of any faster
generally-available engine, then please let me know!).

### Who _isn't_ Monument for?

If you do want to run exhaustive searches or want guaranteed optimal results, then Monument isn't
for you - [SMC](https://github.com/GACJ/smc) is extremely good at exhaustive searching to get
optimal results.  If you want to exhaustively search spliced, I think you need to write your own
engine.  I wish you the best of luck getting results before the eventual heat death of the universe.

If you want a GUI, Monument isn't for you (yet).

If you want an engine that doesn't change often, Monument isn't for you (yet).

## Features

- **FAST**.  Like, really fast.  This specifically means fast at generating compositions _that you
  might want to ring_.
- Easy spliced: Generating spliced is as simple as adding multiple methods.  Various splicing styles
  are supported, such as only changing method at calls.
- Easy multi-parts.  E.g. `part_head = "134265"` will allow `134265` or `142365` (but not `123465`
  or `134256`).
- Short-hands for common things - e.g. `length = "peal"` or `length = "QP"`, or adding runs
  front/back as music:
  ```toml
  [[music]]
  run_lengths = [4, 5, 6, 7, 8]
  ```
- Lots of tuning parameters to make sure Monument understands what you're after:
  - Call weighting to encourage sparse callings.  Give calls a negative score to generate sparse
    callings even if it misses a little bit of music.
  - Add weighting to every row in specific coursing patterns.  For example, encourage
    tenors-together and/or handbell friendly courses.

### Planned/WIP Features

- A GUI to make composition review easier.
- Duffer limits.  E.g. require no more than 3 leads between musical courses.
- Add range requirements to music counts.  E.g. require all 24 5678/8765s.
- Require specific courses/segments.  E.g. require all the `*6578` courses (but not necessarily all
  the 6578s).
- Variable score, to encourage certain types of music in certain places within the composition
- More tuning parameters for e.g. method balance, changes of method, etc.

## Known issues

- Triples methods like Grandsire and Stedman will produce very strange compositions.  Supporting
  these is totally intended, but there are some questions that need answering before either of
  them can fit nicely with Monument's model of ringing.
- Huge memory usage.  This happens due to the way Monument's current search algorithm works.  In
  short, Monument continually keeps track of a big queue of composition prefixes, and repeatedly
  replaces the best prefix with prefixes that are slightly longer.  Storing a large enough queue
  takes a lot of memory, and I haven't implemented a true memory limit yet.  In the mean time,
  adding `-Q <number>` to the end of a command will limit the queue length to `<number>` (the
  default limit is 10,000,000).

## Composing is Hard

Finally, it's worth pointing out that generating compositions is, in general, **really hard** ([NP
hard](https://en.wikipedia.org/wiki/NP-hardness), in fact).  There will always be queries which are
simply too hard for Monument (or any other engine) to complete, so any promise of speed is a
best-effort not a guarantee.
