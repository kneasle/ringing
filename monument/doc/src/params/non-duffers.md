# Non-Duffers

Parameters which control Monument's limits on 'non-duffer' courses.

**Contents:**
- [`non_duffer_courses`](#non_duffer_courses)
- [`max_total_duffer`](#max_total_duffer)
- [`max_consecutive_duffer`](#max_consecutive_duffer)

---

**TODO:** Move this to an intro section, and replace it with an param reference

## `non_duffer_courses`

Specifies which courses are 'non-duffer'.  Courses which don't satisfy anything in
`non_duffer_courses` are considered 'duffers' and `max_{total,contiguous}_duffer` can be used to
restrict how much 'duffer' is allowed in a given composition.

Shorthands for `any_bells` and `any_stroke` can be used to easily specify lots of courses based on
some pattern.  For example, all the following specify the 4-bell run courses in most Major methods:

1. Specifying all courses explicitly:
   ```toml
   non_duffer_courses = [
       "*5678", "*8765", "*6587", "*7856",
       "*4567", "*7654", "*5476", "*6745",
       "*3456", "*6543", "*4365", "*5634",
       "*2345", "*5432", "*3254", "*4523",
   ]
   ```
2. Using `any_bells` to expand the courses with every similar pattern of bells:
   ```toml
   non_duffer_courses = [
       { courses = ["*5678", "*8765", "*6587", "*7856"], any_bells = true },
   ]
   ```
3. Using `any_stroke` to expand music on backstroke to that on handstroke:
   ```toml
   non_duffer_courses = [
       { courses = ["*5678", "*8765"], any_bells = true, any_stroke = true },
   ]
   ```



## Quick note on `non_duffer_courses` and multi-parts

Currently, Monument does not consider the non-duffer status of each part separately.  Thus, a
'chunk' of ringing is a non-duffer if and only if it's a non-duffer in **every** part.  This is a
potential gotcha when combined with e.g. cyclic multi-parts.  For example, if you want
'non-duffers' to be courses which generate 4-bell runs in _some_ part, the first instinct is often
to specify 4-bell run courses like so:

```toml
non_duffer_courses = [{ courses = ["*5678", "*8765"], any_bells = true, any_stroke = true }]
```

This, however, makes only the plain course a non-duffer.  Any other course will generate `*6782` in
some part and therefore all other 4-bell run courses are a duffer in some part.  To get the
expected behaviour, you need to explicitly add `*6782`, `*7823` and `*8234` as non-duffers, like so:

```toml
non_duffer_courses = [
  { courses = ["*5678", "*8765"], any_bells = true, any_stroke = true },
  { courses = ["*6782", "*7823", "*8234"], any_stroke = true } # Not using `any_bells` here
]
```



## `max_total_duffer` and `max_contiguous_duffer`

`max_total_duffer` and `max_contiguous_duffer` specify limits (in number of rows) on how much
'duffer' can be rung.  For example, the following produces QPs of Bristol with at most two leads of
_consecutive_ duffer and at most four leads of _total_ duffer:

```toml
length = "qp"
method = "Bristol Surprise Major"
bobs_only = true

non_duffer_courses = [
    { courses = [ "*5678", "*5x678", "*8765", "*8x765" ], any_stroke = true, any_bells = true },
    "*6578", "*5x678",
]
max_contiguous_duffer = 64 # Limited by contiguous duffers
max_total_duffer = 128
```
