# BellFrame

BellFrame is a standard library for change ringing.

In other words, BellFrame aims to provide high-quality idiomatic implementations of basic things
required for change ringing (e.g. borrowed/owned `Row` types, place notation parsing, method
classification, access to the CC method library, etc.).  These can then be shared between projects
to speed up development and reduce bugs.

The two fundamental goals of BellFrame are **reliability** and **performance**, _in that order_.
Reliability means that BellFrame's behaviour is always well defined and never causes Undefined
Behaviour, even for pathological inputs.  Performance means, specifically, that BellFrame's code
should never be the limiting factor in a project.  However, reliability is more important than
unbeatable performance so I'm not going write complex optimised code until I at least know that
operation is a bottleneck - it's just so much easier to know that simple code is correct.

## Contents

#### Core Data-types:
- `Truth`, `Parity`, `Stroke`: Type-safe versions of `bool`, using the type system to prevent them
  from being mixed up.
- `Stage`, `Bell`: Type-safe representations, again for avoiding common errors like calling the
  treble `1` (useful for humans) or `0` (for indexing into arrays).
- `Row`, `RowBuf`: Borrowed and owned rows (respectively).  Conceptually, `Row` is like `[Bell]`
  and `RowBuf` is like `Vec<Bell>`, except they are both guaranteed to contain valid permutations.

#### Methods
- `Method`, `Call`: Method processing, and classification code.  The classification algorithm is
  100% compliant with the Framework (in implementing it, I discovered bugs in the CompLib's
  classifier, which have now been fixed).
- `MethodLib`: Access to method libraries, with `MethodLib::cc_lib()` loading an up-to-date copy of
  the Central Council's library.  Searching the library is also supported, and also returns
  suggested method names for unsuccessful searches.

#### Block Manipulation:
- `SameStageVec`, `Block`: Ways of storing sequences of `Row`s in a single contiguous chunk of
  memory (thus allowing huge cache-efficiency and optimisation potential).  `SameStageVec` is
  simply a sequence of `Row`s with the same `Stage`; `Block` allows its `Row`s to be given
  annotations of any type.

#### Other Useful Types:
  - `Mask`: Like a `RowBuf`, except that bells can be missing (denoted by `x`).  For example,
    `1xxx5678` is a `Mask`, where 2,3,4 can go in any order.
  - `Regex`: Default way of representing music patterns - like a `Mask`, except that `*` matches any
    number of bells.  For example, `*x7x8x*` is a `Regex`.
