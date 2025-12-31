https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

AHHHHHHHHHHHHHHHHHHHHHHHHHH

- stack and rpn is a good way of thinking about things
- if expr then expr else expr is an expression
- expressions can also be (the precise implementation of this i got from like
  zig, f#, rust)
  ```
  {
    let x = 41 in
    let y = 69 in 
    x+y
  }
  ```
  where instead of in we write ; so we have the familiar
  ```
  {
    let x = 41;
    let y = 69;
    x+y
  }
  ```
  we can also have
  ```
  {
    let x = 41;
    let y = 69;
    x+y;
    x+y;
  }
  ```

  which implicitly is
  ```
  {
    let x = 41; 
    let y = 41;
    let NULL = x+y;
    void
  }
  ```

in a similar way

```
if x < 10 print(x)
```

should be the same as

if x < 10 print(x)

---

I think that `expr ; expr ; expr` should be an expression which should evaluate
to the **type** of the final expression but combine the effect of all the
expressions.

Similarly `expr ; expr ; expr;` should return the type void but combine the
effect of all the expressions before it.

`if expr then expr else expr` is an expression where the type of the two
branches should be the same and the overall effect is the effect of each branch

> this leads to funny looking code like

```
else
  print("the searched item is not in the list")
  found = false;
  score = score - 1;
  void
```

## `while tr`

- added the while (expr) : (expr) statement
- made if else and while else expressions
- made blocks expressions if the last thing is not terminated by a semicolon

- added comparison chaining
- added use python style keywords

- i plan to make labeled breaks but i haven't gotten there yet

**things i must add eventually**:

- piping
- pattern matching / destructuring
- compile-time polymorphism

lexer -> expression parser -> parser -> idk

# Lexing

## List of Tokens

kill myself

# Parsing

## Expression Parsing

```
<expression>       ::= <assignment>

<assignment>       ::= <logical_or>
                     | <unary> "=" <assignment>

<logical_or>       ::= <logical_and>
                     | <logical_or> "or" <logical_and>

<logical_and>      ::= <equality>
                     | <logical_and> "and" <equality>

<equality>         ::= <relational>
                     | <equality> "==" <relational>
                     | <equality> "!=" <relational>

<relational>       ::= <additive>
                     | <relational> "<"  <additive>
                     | <relational> ">"  <additive>
                     | <relational> "<=" <additive>
                     | <relational> ">=" <additive>

<additive>         ::= <multiplicative>
                     | <additive> "+" <multiplicative>
                     | <additive> "-" <multiplicative>

<multiplicative>   ::= <unary>
                     | <multiplicative> "*" <unary>
                     | <multiplicative> "/" <unary>
                     | <multiplicative> "%" <unary>

<unary>            ::= <primary>
                     | "+" <unary>
                     | "-" <unary>
                     | "not" <unary>
                     | "*" <unary>       -- dereference
                     | "&" <unary>       -- address-of

<primary>          ::= <identifier>
                     | <number>
                     | "(" <expression> ")"
                     | <primary> "[" <expression> "]"        -- array indexing
                     | <primary> "(" <argument_list> ")"     -- function call
                     | <primary> "." <identifier>            -- struct member
                     | <block_expr>

<block_expression> ::= "{" <block_inside> "}"
<block_inside>     ::= <expression>
                     | <statement> <block_inside> 

<if_expression>    ::= "if" "(" <expression> ")" <expression> else <expression
<while_expression> ::= "while" "(" <expression> ")" <expression> else "(" <expression> ")"

<argument_list>    ::= <expression>
                     | <expression> "," <argument_list>
                     | ε

<identifier>       ::= [A-Za-z_][A-Za-z0-9_]*
<number>           ::= [0-9]+ ("."[0-9]+)?
```

## Statement Parsing

```
<stmt> ::= <compound_stmt>
         | <if_stmt>
         | <while_stmt>
         | <return_stmt>

<compound_stmt> ::= "{" <stmt_list> "}"

<stmt_list> ::= <stmt> <stmt_list>
              | ε

<if_stmt> ::= "if" "(" <expr> ")" <stmt> [ "else" <stmt> ]

<while_stmt> ::= "while" "(" <expr> ")" [ ":" "(" <expr> ")" ] <stmt>

<return_stmt> ::= "return" <expr> ";"
```

<!-- TODO: i fucking hate my life -->

## Type Parsing

```
<type>
    ::= <identifier>
     |  "*" <type>
     |  "[" <int> "]" <type>
     |  "fn" "(" <type_list>? ")" <type>
     |  <identifier> "[" <type_list> "]"
```

because the type family is "indexed" by types :sob:

## Declarations

```
<program> ::= <declaration>*
```

```
<declaration>
    ::= <function_decl>
     |  <variable_decl>
     |  <struct_decl>
     |  <type_decl>

<param_list>
    ::= <param>
     |  <param> "," <param_list>

<param>
    ::= <identifier> ":" <type>
```
