# elm-math-parser

Parse and evaluate basic math expressions involving +, -, *, /, ^, and () using [elm/parser](https://github.com/elm/parser). Offers comprehensive error messages.

# How it works
Elm-math-parser uses recursive descent or top-down parsing to convert raw strings into an Abstract Syntax Tree (AST). Math expressions in the AST can be any of the 8 following variations:

```elm
type Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Exp Expr Expr
    | Neg Expr
    | Literal Float
    | Grouping Expr
```

The parsed AST is then evaluated to produce the result of the expression.

# Usage
```elm
import MathParser exposing (parse, evaluate, displayError)

eval : String -> Value
eval source =
    case parse source of
        Err err ->
            Err (displayError source err)
        Ok expression ->
            Ok (evaluate expression)

_ =
    eval "3 - (4 * 5)" -- yields -17

_ =
    eval "0.82 ^ -3.22 / (2.005 + 3)" -- yields 0.378543082

_ =
    eval "1 + +4"
    -- yields error message:
    -- 1 + +4
    --     ^
    -- Expecting a "(" or a real number
```

# License
MIT
