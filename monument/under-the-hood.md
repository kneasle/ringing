# Under the Hood

Under the hood, Monument's architecture is very similar to that of SMC32.

A composition search consists of three phases:

1. The query is parsed from the input TOML file and validated.

2. [A **composition graph**](#the-composition-graph) is built.  This contains every atomic 'chunk'
   of composition, along with how these can be 'linked' together (with calls, splices, or plain
   leads).  Finally, this also records which chunks are false against each other.  Compositions are
   now represented as _paths_ through the graph.

3. Monument runs [best-first tree search](#best-first-tree-search) over the possible **paths**
   through the composition graph.  This works by progressively extending short compositions (which
   will likely not have come round yet) until eventually an 'end' link is found and the composition
   comes round.  Monument then checks that this new composition satisfies the user's requirements -
   it is the right length, it has the right method balance, reaches a valid part head, etc.).  If
   all the constraints are satisfied, the composition is printed to the user's screen.

## The Composition Graph

The composition graph is how Monument represents the space of possible compositions and, as such,
is the heart of Monument.

Monument's composition graph is very similar to that of SMC32, with the extra complexity of handling
spliced and explicit multiparts.  Generating this graph efficiently and correctly, handling all
ringing's edge cases, is challenging enough that a significant proportion of Monument's code is
solely involved in graph building.  However, once the graph is built, it abstracts away much of the
details of ringing and provides a representation of compositions which is incredibly succinct and
easy to modify and reason about.

#### Diagram of an Example Graph

Suppose we ask Monument for a composition of Yorkshire Major in which the calls are all Homes:

```toml
length = { min = 0, max = 2000 }
method = "Yorkshire Surprise Major"
courses = "*5678" # Force 5678 to be unaffected, restricting us to only Homes
```

This will generate a composition graph like this.  Note that every box is **an entire course**:

![Diagram of Composition Graph](img/graph-combined.png)

Also note how compositions now corresponds to paths through this graph:

![Paths through the Composition Graph](img/comp-graph-paths.png)

#### Components of a Composition Graph

A composition graph is made of three major components:

1. **Chunks.**  A chunk corresponds to a single atomic (i.e. unsplittable) section of ringing.  In
   other words: once we start ringing a chunk, we must ring it to completion before making the next
   decision in the composition.

   For example, in 'normal' spliced, we always have to ring whole leads of any method but a splice
   can occur at every lead, so each chunk in the resulting graph will correspond to a single
   lead.  In other compositions, a single chunk could contain many leads; for example, in
   tenors-together Yorkshire Royal, we cannot make a call during the four leads between Home and
   Middle, so these four leads would form a single chunk.

   Mathematically, these are the _nodes_ in the composition graph.

2. **Links.**  A link denotes that it is possible for one chunk to follow another, and correspond
   to _decisions_ in the composition.  A link could correspond to a call, a method splice
   or a decision to do neither (i.e. a 'plain' link).

   Starting and ending a composition is also represented as links.  These are links where one end
   doesn't correspond to a chunk - instead it points to a magic 'start' or 'end' entity.

   Mathematically, these are _directed edges_ in the composition graph.

3. **Falseness links.**  A falseness link joins two chunks which are mutually false.  Specifically,
   a falseness link from chunk A to chunk B signals that ringing chunk A means that chunk B can no
   longer be rung.
  
   Therefore, all chunks have falseness links to themselves - because ringing a chunk means that it
   can no longer be rung again.  Truly self-fase chunks (ones which contain a repeated row) are
   completely removed from the graph during construction.

   Mathematically, these form an _undirected graph_ over the same nodes as the other graph.

## Best-First Tree Search

To perform tree search, Monument stores a queue of prefixes which it is interested in expanding.
Every iteration of the core composing loop, Monument picks a prefix from the queue and extends
it.  This creates a new prefix for every possible action at that point (e.g. new prefixes could
be created for 'call a bob', 'call a single', 'choose to call nothing').  These new prefixes are
returned to the queue and a new iteration begins.

If the prefix being expanded has the magic 'end' entity as a next step, the prefix has come round
and becomes a full composition.
A `Composition` struct is created for this new composition and is validated against the constraints
of the composition (e.g. is it long enough, does it have a valid method balance, etc.).  If it is
valid, the composition is sent down a channel to the printing thread and printed to the user's
screen.

Exactly how the new prefix is chosen for expansion is the secret sauce which makes Monument so
fast, and is incredibly simple:  Monument will choose to expand the prefix which has **the highest
score per row**.

This is the key to how Monument is so fast - it prioritises exploring the composition ideas
which are most likely to eventually create good compositions.  It doesn't matter if the core
composing loop is hundreds of times slower than SMC's; Monument needs to run that loop many, many
orders of magnitude fewer times before very good compositions are generated.

However, the cost of this is that Monument must store many, many, many prefixes in the queue
in order to have different ideas to explore (the queue will often contain _tens of millions_ of
prefixes).  Not only does this require a huge amount of memory (meaning it is important to minimise
the memory footprint of each prefix) - but this also puts heavy pressure on the memory allocator
(meaning that Monument generally relies on pre-allocating blocks of memory for data which changes
often).
