# `method` and `methods`

Methods can be added by using either the `method` or `methods` parameters.

_**Using `method` is exactly equivalent to setting `methods` to a one-element array.**_

So, this:
```toml
length = "QP"
method = "Bristol Surprise Major"
```
is completely equivalent to this:
```toml
length = "QP"
methods = ["Bristol Surprise Major"]
```

You can add methods to Monument by either loading an existing method by title, or loading a custom
method directly from place notation.



## Load an Existing Method

Existing methods are loaded from the Central Council's Method Library (the one which powers
[CompLib](complib.org)).

For this, we use the `title` parameter:
```toml
length = "QP"
method = { title = "Bristol Surprise Major" }
```

In fact, this is such a common way to specify methods, that Monument lets you elide the
`title` parameter and specify the title directly:
```toml
length = "QP"
method = "Bristol Surprise Major"
```



## Load a Custom Method

To ring a method that isn't in the Central Council's library, you can add methods to Monument
directly from place notation.  For this, we need to specify the method's `place_notation` and its
`stage`.  We also specify the method's `name`:

```toml
length = "QP"
method = { name = "Bristol", place_notation = "x5x4.5-5.36.4-4.5x4x1,8", stage = 8 }
```

**Note:** we are not specifying the *title* here.  Monument will classify the method for you and
generate its title automatically.  In this case, Monument classifies 'Bristol' as a 'Surprise'
method and gives it the title 'Bristol Surprise Major'.



## Summary

To summarise, these are all precisely equivalent ways of specifying
[Bristol Surprise Major](https://complib.org/method/19048):
```toml
method = "Bristol Surprise Major"
method = { title = "Bristol Surprise Major" }
method = { name = "Bristol", place_notation = "x5x4.5-5.36.4-4.5x4x1,8", stage = 8 }

methods = ["Bristol Surprise Major"]
methods = [{ title = "Bristol Surprise Major" }]
methods = [{ name = "Bristol", place_notation = "x5x4.5-5.36.4-4.5x4x1,8", stage = 8 }]
```
