module Main exposing (main)

import Html exposing (Html)
import Element exposing (column, text, padding)
import Element.Font
import MathParser exposing (parse, evaluate, displayError)

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
