# `graph_size_limit`

**_(since v0.11.0)_**

**Defaults to 100,000.**

Sets a limit on the number of chunks in the composition graph, to prevent Monument from exhausting
the system memory trying to explore a vast search space (e.g. split-tenors Maximus).  Doing so will
cause an error, and this value can be raised manually to explicitly allow Monument to explore
further.
