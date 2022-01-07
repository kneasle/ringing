# Monument

A fast and flexible composing engine for
[method ringing](https://en.wikipedia.org/wiki/Method_ringing).

## CLI Guide

Currently Monument has an command-line interface, much like that of
[SMC](https://github.com/GACJ/smc), where Monument reads files in [TOML](toml.io/) format and prints
results to the console.  A GUI is in progress and mostly obsolete the CLI version but nonetheless, a
guide for the CLI's TOML format can be found [here](cli/guide.md).

## Goals

Goals are roughly ordered with most important first.

- **Correct**: Monument should always produce compositions which satisfy your requirements, without
  crashes or unexpected behaviour.
- **Fast**: Monument should complete most searches in the order of seconds to minutes.  Monument persues this goal
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
