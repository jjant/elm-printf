module Printf exposing
    ( printf
    , c, s, i, f
    , ap
    )

{-| This package provides a type-safe implementation of the famous printf function.

@docs printf


## Format

@docs c, s, i, f


## Combining formats

@docs ap

-}


{-| Combine two formats.

      printf (c "Some" |> ap (c " string")) == "Some string"

      printf (c "My name is " |> ap s) "Jon" == "My name is Jon"

-}
ap : ((String -> b) -> a) -> ((String -> a) -> c) -> (String -> b) -> c
ap f2 f1 k =
    f1 (\s1 -> f2 (\s2 -> k (s1 ++ s2)))


{-| The constant format. Takes a string which printf prints as is.
printf (c "Something...") == "Something..."
-}
c : String -> (String -> a) -> a
c str k =
    k str


trimDigits : Int -> Float -> String
trimDigits precision num =
    (num * toFloat (10 ^ precision))
        |> round
        |> toFloat
        |> (\v -> v / toFloat (10 ^ precision))
        |> String.fromFloat


{-| The Float format. Passing it to printf means you expect a Float.
Takes an argument being the number of decimals to show.

      printf (f 5) 1.0000123 == "1.00001"

-}
f : Int -> (String -> a) -> Float -> a
f precision k flt =
    k (trimDigits precision flt)


{-| The String format. Passing it to printf means you expect a String.

      printf s "Hey" == "Hey"

-}
s : (String -> a) -> String -> a
s k str =
    k str


{-| The Int format. Passing it to printf means you expect an Int

      printf i 23 == "23"

-}
i : (String -> a) -> Int -> a
i k int =
    k (String.fromInt int)


{-| The printf function, takes a format and returns the appropiate type.

      printf (c "Hey") == "Hey"

      printf s "Some string" == "Some string"

-}
printf : ((a -> a) -> b) -> b
printf fmt =
    fmt identity
