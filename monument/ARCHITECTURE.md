# Architecture of Monument's Code

This document provides a mental map of Monument's code.  It aims to provide four things for
newcomers to the code:

1. [\[link\]](#overview-of-monument) An overview of Monument.
2. [\[link\]](#data-flow) A description of how data flows through the code during a single
   composition search.
3. [\[link\]](#code-map) An answer to the question: _'What does X code do?'_

---

## Overview of Monument

As stated in the README file, Monument is a composition _generator_.  I.e. it can be thought of as
a computer composer: you provide it with a description of what compositions you want and it
generates the best compositions it can, according to your definition of 'good'.

The ultimate goal of Monument is to **get you compositions that you want to ring, as quickly and
easily as possible**.

Note that this doesn't necessarily mean generating the perfect composition, or generating _every
possible_ composition.  Monument guarantees neither of these but in return, Monument is **orders of
magnitude faster** than any other composing program I'm aware of (if anyone knows of any faster
generally-available program, then please let me know!).

A general overview of Monument's internals can be found [here](under-the-hood.md).

---

## Data Flow

This section describes the data flow through Monument.

Monument's architecture is similar to that of multi-pass compilers: Data is processed by converting
it between increasingly detailed and abstract data types. 

The overall flow of each run of Monument consists of three major steps, each of which is given a
section below:

1. [**Parsing and Validation**](#stage-1-parsing-and-validation)
2. [**Graph Building**](#stage-2-graph-building)
3. [**Composition Search**](#stage-3-composition-search)

### Stage 1: Parsing and Validation

The first step for Monument is to read and validate the information stored in the TOML file.  This
begins with a TOML file on disk, and finishes with an instance of `monument::query::Query` (which
contains the same data, but in a strongly typed and validated form).

This currently requires multiple steps, as shown below.  The structures of of these data types
(`monument_cli::toml_file::TomlFile`, `monument::SearchBuilder` and `monument::query::Query`)
is largely recognisable as the same format as the TOML file.  However, each step performs
additional validation and, where needed, converts the data into Monument's internal data types.

```text
            +-----------+
            | TOML File |         (on disk)
            +-----------+
                  |
                  |
               parsing       (handled by the `serde` and `toml` libraries)
                  |
                  V
        +-------------------+
        | `TomlFile` struct |     (found in `cli/src/toml_file.rs`)
        +-------------------+
                  |
                  |
             validation      (handled by `TomlFile::to_search`)
                  |
                  V
      +------------------------+
      | `SearchBuilder` struct |  (found in `lib/src/builder/mod.rs`)
      +------------------------+
                  |
                  |
          more validation    (handled by `SearchBuilder::build`)
                  |
                  V
         +----------------+
         | `Query` struct |       (found in `lib/src/query.rs`)
         +----------------+
```

### Stage 2: Graph Building

As described in section on [The Composition Graph](#the-composition-graph), much of the code of
Monument is involved with building an abstract graph of all possible pieces of the composition and
how they are linked together.

There are actually two graph types in Monument:

1. `monument::graph::Graph` in **lib/src/graph/mod.rs**, which is the first graph to be built.
   This graph stores the chunks in a `HashMap` and is easy and efficient to modify, but
   accessing each chunk is relatively slow (involving a `HashMap` lookup).
2. `monument::search::Graph` in **lib/src/search/graph.rs**, which is copied from a
   `monument::graph::Graph` while setting up for the search.  This stores the nodes in a flat array
   (i.e. a `Vec`tor) and is very fast to access but is almost impossible to modify.

The overall graph building flow is as follows:

```text
         +----------------+
         | `Query` struct |              (found in `lib/src/query.rs`)
         +----------------+
                  |
                  |
          layout generation      (found mainly in `lib/src/graph/build/layout.rs`)
                  |
                  V
 +-----------------------------------+
 | `HashMap<ChunkId, PerPartLength>` |   (which chunks are possible, and how long they are)
 +-----------------------------------+
                  |
                  |
         atw table generation   (found in `lib/src/atw.rs`)
                  |
        falseness calculation   (found in `lib/src/graph/build/falseness.rs`)
                  |
            music counting
                  |
                  V
  +---------------------------------+
  | `monument::graph::Graph` struct |     (found in `lib/src/graph/mod.rs`)
  +---------------------------------+
                  |
             optimization
                  |
                  V
  +---------------------------------+
  | `monument::graph::Graph` struct |
  +---------------------------------+
                  |
                  |
              copy data
                  |
                  V
  +----------------------------------+
  | `monument::search::Graph` struct |     (fast access graph, found in `lib/src/search/graph.rs`)
  +----------------------------------+
```


### Stage 3: Composition Search

Once the fast access, optimized graph is created, Monument can begin performing tree search over
compositions, which are represented as increasingly long paths through the composition graph.
These are referred to as composition `Prefix`es, since they correspond to the first part of
complete composition.  A completed composition is stored as a `Composition` struct.

All of this code is found in the `lib/src/search` directory.

The overall flow of the search algorithm is as follows:

```text
  +----------------------------------+
  | `monument::search::Graph` struct |
  +----------------------------------+
                  |
                  |
                  V
         +-----------------+
         | `Prefix` struct |           (found in `lib/src/search/prefix.rs`)
         +-----------------+
                  |
                  |
          repeated expansion     (found in `lib/src/search/best_first.rs`)
                  |
                  V
         +-----------------+
         | `Prefix` struct |
         +-----------------+
                  |
                  |
       composition comes round
                  |
              validation          (found in the `Prefix::expand` function)
                  |
                  V
       +----------------------+
       | `Composition` struct |   (found in `lib/src/composition.rs`)
       +----------------------+
                  |
                  |
          sent down channel
                  |
           pretty printing        (found in `cli/src/logging.rs`)
                  |
                  V
      +------------------------+
      | User's screen (stdout) |
      +------------------------+
```

----

## Code Map

### Command-Line Interface

```text
cli/src
├── lib.rs            (crate root)
├── main.rs           (entry point of Monument)
├── args.rs           (parsing of CLI arguments)
│
├── toml_file.rs      |
├── calls.rs          | Specification of the CLI interface
├── music.rs          |
│
├── logging.rs        (pretty printing of compositions)
│
├── default-music-major.toml    |
├── default-music-maximus.toml  |
├── default-music-minor.toml    | Default music files
├── default-music-royal.toml    |
├── default-music-triples.toml  |
│
└── utils.rs          (misc utilities)
```

### Core (non-CLI-specific) Library

```text
lib/src
├── lib.rs               (crate root)
│
├── builder              ('builder' API for constructing search queries)
│  ├── mod.rs
│  └── methods.rs
├── query.rs             (fully validated description of a search query)
├── composition.rs       (a generated composition)
│
├── graph
│  ├── mod.rs            (definition of the easy-to-modify graph)
│  │
│  ├── build
│  │  ├── falseness.rs   |
│  │  ├── layout.rs      | Graph building
│  │  └── mod.rs         |
│  │
│  └── optimise
│     ├── mod.rs         |
│     ├── music.rs       | Graph optimization (removing unneeded sections of the graph)
│     └── strip_refs.rs  |
│
├── search               (search routine)
│  ├── mod.rs
│  ├── best_first.rs     (code for best-first search)
│  ├── graph.rs          (quick-access graph)
│  ├── prefix.rs         (composition prefixes, including expansion thereof)
│  └── path.rs           (efficient storage of paths through the composition graph)
│
├── error.rs             (errors returned by Monument)
├── atw.rs               (efficient calcuation of atw)
├── group.rs             (part head group)
├── prove_length.rs      (proves which lengths and method counts are possible)
│
└── utils
   ├── counts.rs         |
   ├── lengths.rs        | Misc utilities
   └── mod.rs            |
```
