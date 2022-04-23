# An Interesting Title

These test cases test that all the default music files are well-formed.

## minor
```toml
length = "practice"
method = "Norwich Surprise Minor"
```

## triples
```toml
length = { min = 0, max = 150 }
method = "Plain Bob Triples"
num_comps = 150 # 124 comps are possible
```

## major
```toml
length = "practice"
method = "Bristol Surprise Major"
```

## royal
```toml
length = "QP"
method = "Yorkshire Surprise Royal"

[[calls]]
symbol = "x"
place_notation = "16"
weight = -5
```

## maximus
```toml
length = 1584 # 3 courses
method = "Yorkshire Surprise Maximus"
```
