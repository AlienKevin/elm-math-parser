module Main exposing (main)

import Html exposing (Html)
import Element exposing (column, text, padding)
import Element.Font
import Parser exposing (..)

sourceCode : String
sourceCode =
    "2 * ( -4 / -4 + 2 ) - 4"
    -- "-2 * - 6"

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

        Grouping l ->
            evaluate l

        Literal l ->
            l


type Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
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


add : Parser Expr
add =
    succeed
        binary
        |= mul
        |. spaces
        |= oneOf
            [ succeed (Operand AddOp)
                |. symbol "+"
                |. spaces
                |= lazy (\_ -> add)
            , succeed (Operand SubOp)
                |. symbol "-"
                |. spaces
                |= lazy (\_ -> add)
            , succeed NoOperand
            ]


mul : Parser Expr
mul =
    succeed
        binary
        |= primary
        |. spaces
        |= oneOf
            [ succeed (Operand MulOp)
                |. symbol "*"
                |. spaces
                |= lazy (\_ -> mul)
            , succeed (Operand DivOp)
                |. symbol "/"
                |. spaces
                |= lazy (\_ -> mul)
            , succeed NoOperand
            ]


primary : Parser Expr
primary =
    oneOf
        [ grouping
        , real
        ]


real : Parser Expr
real =
    succeed
        (\op num ->
            Literal
                (case op of
                    Nothing ->
                        num

                    Just _ ->
                        negate num
                )
        )
        |= oneOf
            [ succeed Just
                |= symbol "-"
            , succeed Nothing
            ]
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
