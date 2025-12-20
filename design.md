some of the changes i made to c

- added the while (expr) : (expr) statement
- removed -> and a bunch of stuff
- added comparison chaining
- added use python style keywords

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
