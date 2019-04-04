module Example exposing (t1)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Printf exposing (..)
import Test exposing (..)


t1 : Test
t1 =
    describe "printf"
        [ test "with int" <|
            \_ ->
                printf i 23
                    |> Expect.equal "23"
        , test "with string" <|
            \_ ->
                printf s "Hey"
                    |> Expect.equal "Hey"
        , test "with const combined with string" <|
            \_ ->
                printf (c "Hey " |> ap s) "Jon"
                    |> Expect.equal "Hey Jon"
        , test "with float no decimals" <|
            \_ -> printf (f 0) 1.0000123 |> Expect.equal "1"
        , test "with float 5 decimals" <|
            \_ -> printf (f 5) 1.0000123 |> Expect.equal "1.00001"
        , test "complex" <|
            \_ ->
                printf (s |> ap (c " said ") |> ap s |> ap (c " is ") |> ap i |> ap (c " years old.")) "John" "Mary" 23
                    |> Expect.equal "John said Mary is 23 years old."
        ]
