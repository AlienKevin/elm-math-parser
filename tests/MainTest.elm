module MainTest exposing (..)

import Test exposing (..)
import Expect
import MathParser exposing (parse, evaluate, displayError)


type alias Value =
    Result String Float


calc : String -> Value
calc source =
    case parse source of
        Err err ->
            Err (displayError source err)
        Ok expression ->
            Ok (evaluate expression)


equal : Value -> String -> Expect.Expectation
equal expected source =
    case calc source of
        Err err ->
            case expected of
                Err expErr ->
                    Expect.equal expErr err
                Ok _ ->
                    Expect.fail ("Evaluation results in an error:\n" ++ err)
        Ok result ->
            case expected of
                Err _ ->
                    Expect.fail ("Evaluation results in:\n" ++ String.fromFloat result)
                Ok expResult ->
                    Expect.within (Expect.Absolute 0.000000001) expResult result


suite : Test
suite =
    describe "Binary operations"
        [ describe "Add and Subtract"
            [ test "a + b 1" <|
                \_ ->
                    equal (Ok 5) "1 + 4"
            , test "a + b 2" <|
                \_ ->
                    equal (Ok 1) "1 + 0"
            , test "a + b 3" <|
                \_ ->
                    equal (Ok -3) "1.23 + -4.23"
            , test "a - b + c" <|
                \_ ->
                    equal (Ok 0) "0 - 2 + 2"
            , test "a - (b + c)" <|
                \_ ->
                    equal (Ok -4) "0 - (2 + 2)"
            ]
        , describe "Multiply and Divide"
            [ test "a * b 1" <|
                \_ ->
                    equal (Ok 8) "2 * 4"
            , test "a * b 2" <|
                \_ ->
                    equal (Ok 0) "1 * 0"
            , test "a * b 3" <|
                \_ ->
                    equal (Ok -5.2029) "1.23 * -4.23"
            , test "a * b * c" <|
                \_ ->
                    equal (Ok 0.01032) "-2 * -2.58 * 0.002"
            , test "a / b * c" <|
                \_ ->
                    equal (Ok 0.0015503876) "-2 / -2.58 * 0.002"
            ]
        , describe "Exponentiation"
            [ test "a ^ b" <|
                \_ ->
                    equal (Ok 8) "2 ^ 3"
            , test "a ^ b ^ c" <|
                \_ ->
                    equal (Ok 262144) "4 ^ 3 ^ 2"
            , test "(a ^ b) ^ c" <|
                \_ ->
                    equal (Ok 4096) "(4 ^ 3) ^ 2"
            ]
        , describe "Negation"
            [ test "-a" <|
                \_ ->
                    equal (Ok -3.0038) "-3.0038"
            , test "-a - b" <|
                \_ ->
                    equal (Ok -3.01) "-0.01 - 3"
            , test "a - -b" <|
                \_ ->
                    equal (Ok 3.02) "0.01 - -3.01"
            , test "-(-a - b)" <|
                \_ ->
                    equal (Ok 5) "-(-2.4 - 2.6)"
            ]
        ]