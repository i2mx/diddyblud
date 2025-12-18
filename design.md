```
range(5) |> combinations(?,2) |> filter((x) => sum(x) == 5)
```

```
|> range(5) // range(5) is an iterator
|> combinations(?,2) // combinations is a template which converts an iterator[a] to another iterator[a]
// we can see that it is partially applied with the empty spot being ?
|> filter( (x) => sum(x) == 5 )
// filter is a template which converts an iterator to another iterator
// (x) => sum(x) == 5 is an anonymous template
```

```
Add = (x, y) => x + y
// why not
Add(x,y) = x + y

// partial application

Increment = Add(1,?)
// this is another template which is equivalent to
Increment(x) = 1 + x
// this is known at compile time 

We only expand the template once all arguments are known
5 |> Increment
// expands to 1 + 5
```

# List of tokens

```
mathematics
     ( ) + - *
template creation
     ( , ) =>
template expansion
     ( ? , )
```

# Expression Parsing

```
expr = (identifier, ...) => (expr)
     | (expr)(expr,...)...
     | (expr)
     | identifier (expr, expr, ...)
     | identifier
     | literals
     | expr * expr
     | expr + expr
```
