module Main exposing (main, parse, evaluate, displayError)

import Html exposing (Html)
import Element exposing (column, text, padding)
import Element.Font
import Parser exposing (..)

sourceCode : String
sourceCode =
    -- "2 * ( -4^2 / -4 + 2 ) - 4"
    -- "-2 * - 6"
    -- "2 ^ 3 ^ 2"
    -- "-3 ^ 2"
    -- "0 - 2 + 2"
    "-2 / -2.58 * 0.002"

main : Html msg
main =
    Element.layout
        [ padding 15
        , Element.Font.family
            [ Element.Font.typeface "Consolas"
            , Element.Font.monospace
            ]
        ]
        (case parse sourceCode of
            Err err ->
                column []
                    [ text (displayError sourceCode err)
                    ]

            Ok expression ->
                column []
                    [ text (Debug.toString expression)
                    , text (Debug.toString (evaluate expression))
                    ]
        )

displayError : String -> List DeadEnd -> String
displayError source errs =
    case List.head errs of
        Nothing ->
            ""

        Just firstErr ->
            source
                ++ "\n"
                ++ String.repeat (firstErr.col - 1) " "
                ++ "^"
                ++ "\nExpecting "
                ++ String.join
                    " or "
                    (List.map displayExpected errs)


displayExpected : DeadEnd -> String
displayExpected err =
    case err.problem of
        ExpectingNumber ->
            "a real number"
        ExpectingSymbol s ->
            "a \"" ++ s ++ "\""
        _ ->
            "an expression"


evaluate : Expr -> Float
evaluate expression =
    case expression of
        Add a b ->
            evaluate a + evaluate b

        Sub a b ->
            evaluate a - evaluate b

        Mul a b ->
            evaluate a * evaluate b

        Div a b ->
            evaluate a / evaluate b

        Exp a b ->
            evaluate a ^ evaluate b

        Neg a ->
            negate (evaluate a)

        Grouping l ->
            evaluate l

        Literal l ->
            l


type Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Exp Expr Expr
    | Neg Expr
    | Literal Float
    | Grouping Expr


parse : String -> Result (List DeadEnd) Expr
parse source =
    run expr source


expr : Parser Expr
expr =
    add


type Operator
    = MulOp
    | DivOp
    | AddOp
    | SubOp
    | ExpOp


type Operand
    = NoOperand
    | Operand Operator Expr


binary : Expr -> Operand -> Expr
binary a b =
    case b of
        NoOperand ->
            a

        Operand op e ->
            case op of
                MulOp ->
                    Mul a e

                DivOp ->
                    Div a e

                AddOp ->
                    Add a e

                SubOp ->
                    Sub a e

                ExpOp ->
                    Exp a e


add : Parser Expr
add =
    succeed
        foldBinary
        |= mul
        |. spaces
        |= loop [] addHelper


foldBinary : Expr -> List Operand -> Expr
foldBinary left operands =
    List.foldr
    (\operand expression -> binary expression operand)
    left
    operands


addHelper : List Operand -> Parser (Step (List Operand) (List Operand))
addHelper operands =
    oneOf
        [ succeed (\right -> Loop (Operand AddOp right :: operands))
            |. symbol "+"
            |. spaces
            |= lazy (\_ -> mul)
        , succeed (\right -> Loop (Operand SubOp right :: operands))
            |. symbol "-"
            |. spaces
            |= lazy (\_ -> mul)
        , succeed ()
            |> map (\_ -> Done operands)
        ]


mul : Parser Expr
mul =
    succeed
        foldBinary
        |= exp
        |. spaces
        |= loop [] mulHelper


mulHelper : List Operand -> Parser (Step (List Operand) (List Operand))
mulHelper operands =
    oneOf
        [ succeed (\right -> Loop (Operand MulOp right :: operands))
            |. symbol "*"
            |. spaces
            |= lazy (\_ -> exp)
        , succeed (\right -> Loop (Operand DivOp right :: operands))
            |. symbol "/"
            |. spaces
            |= lazy (\_ -> exp)
        , succeed ()
            |> map (\_ -> Done operands)
        ]


exp : Parser Expr
exp =
    succeed
        binary
        |= primary
        |. spaces
        |= oneOf
            [ succeed (Operand ExpOp)
                |. symbol "^"
                |. spaces
                |= lazy (\_ -> exp)
            , succeed NoOperand
            ]


primary : Parser Expr
primary =
    succeed (\op literal ->
            case op of
                Nothing ->
                    literal

                Just _ ->
                    Neg literal
        )
    |= oneOf
        [ succeed Just
            |= symbol "-"
        , succeed Nothing
        ]
    |= oneOf
        [ grouping
        , real
        ]


real : Parser Expr
real =
    succeed Literal
        |= number
            { int = Just toFloat
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just identity
            }


grouping : Parser Expr
grouping =
    succeed Grouping
        |. symbol "("
        |. spaces
        |= lazy (\_ -> expr)
        |. spaces
        |. symbol ")"
