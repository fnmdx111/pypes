Pypes
====

Pypes is a scripting language that focuses on combining functions using pipes. It is a dynamically typed language that compiles down to Python.

Examples
====

Quick sort
----

```
let qsort input = input
    |[]> const []
  , |[x]> const [x]
  , |pivot:xs> xs |> filter (< pivot) |> qsort |part1>
               const (xs |> filter (>= pivot) |> qsort |part2>
                 const (part1 + [pivot] + part2))
```

This (should) compiles to the following Python code

```python3
def qsort(input):
    def match1(arg):
        def pipe_fun(x):
            return const([])

        if arg == []:
            matched = arg
            return pipe_fun(matched)
        else:
            return None

    def match2(arg):
        def pipe_fun(x):
            return const([x])

        if len(arg) == 1:
            matched = arg[0]
            return pipe_fun(matched)
        else:
            return None

    def match3(arg):
        def pipe_fun(pivot, xs):
            def pipe1(xs):
                def fun1(__arg1):
                    return __arg < pivot
                return filter(fun1, xs)
            def pipe2(__arg1):
                return qsort(__arg1)
            def pipe3(part1):
                ...

        if len(arg) > 1:
            x, *xs = arg
            return pipe_fun(x, xs)

    m = match1(input)
    if not m:
        m = match2(input)
        if not m:
            m = match3(input)
            if not m:
                throw NoMatchError
            else return m
        else:
            return m
    else:
        return m
```
