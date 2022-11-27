# Courses

Parameters which specify which courses can be used in the generated compositions.

**Contents:**
- [`part_head`](#part_head)
- [`courses`](#courses)
- [`split_tenors`](#split_tenors)
- [`course_weights`](#course_weights)
- [`handbell_coursing_weight`](#handbell_coursing_weight)
- ~~[`leadwise`](#leadwise)~~ (removed in v0.10.0)

---



## `part_head`

A row which determines which part heads will be generated.  Note that Monument can generate
compositions with a different part head, provided the same set of parts are generated (so
`part_head = "23456781"` and `part_head = "81234567"` are equivalent but `part_head = "56781234"` is
not).  Defaults to rounds (i.e. one part, or `part_head = ""`).



## `courses`

List of masks which define the courses that Monument can use.  Defaults to tenors together, or any
course (if `split_tenors` is set).  For example:
```toml
courses = ["*78", "xxxx7856", "12345xxx"]
```



## `split_tenors`

If `courses` isn't specified, this lets Monument use any courses, as opposed to just those with
the tenors together.  On higher stages, this will almost certainly cause Monument's graph size limit
to be reached.  Defaults to `false`.



## `course_weights`

Applies some score to every row in a course which contains a lead head which matches a given mask.
For example, the following will weight Monument to create compositions where handbell pairs are
coursing often:
```toml
[[course_weights]]
patterns = [
    "*78",
    "*56", "*65",
    "*34", "*43",
] # can also use e.g. `pattern = "*78"`
weight = 0.05 # this is small because the weight is applied per row
```



## `handbell_coursing_weight`

Generates `course_weights` which apply the given weight to every row where a handbell pair coursing
(this score gets multiplied for courses with multiple handbell pairs coursing).  Equivalent to
something like this (truncated according to stage):
```toml
[[course_weights]]
patterns = [
    "*12", "*21",
    "*34", "*43",
    "*56", "*65",
    "*78", "*87",
    "*90", "*09",
        ...
]
```
Defaults to 0.



## ~~`leadwise`~~

**_(removed in v0.10.0)_**

If set, this will stop Monument using calling positions, and instead label all the calls
positionally.  `courses`, `split_tenors` and `course_weights` will obviously have no effect, and
this will always generate split-tenors compositions.  You should rarely have to set this yourself; by
default, Monument will set this automatically if the tenor is affected by the part head (e.g. in
cyclic) but otherwise will stick to course-wise compositions.  The only times you're likely to need
this is for weird cases like differential methods, which don't have a well-defined concept of a
'course head'.
