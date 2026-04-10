module Library.BytesExtra exposing (toIntArray)

{-| Generic helpers for working with `elm/bytes` values.

@docs toIntArray

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


{-| Convert a `Bytes` value into an `Array Int`, one element per byte
(each value in the range 0-255). This materialises the entire buffer so
callers get O(log32 n) random-access reads, at the cost of higher memory
overhead than the packed `Bytes` form.

Always produces an array of length `Bytes.width raw`. The underlying
`Bytes.Decode` pipeline cannot fail for this decoder, so the result is
returned directly rather than wrapped in `Maybe`.

-}
toIntArray : Bytes -> Array Int
toIntArray raw =
    let
        count =
            Bytes.width raw
    in
    Decode.decode (loopDecoder count) raw
        |> Maybe.withDefault Array.empty


loopDecoder : Int -> Decode.Decoder (Array Int)
loopDecoder count =
    Decode.loop ( 0, Array.empty ) (step count)


step : Int -> ( Int, Array Int ) -> Decode.Decoder (Decode.Step ( Int, Array Int ) (Array Int))
step count ( index, acc ) =
    if index >= count then
        Decode.succeed (Decode.Done acc)

    else
        Decode.unsignedInt8
            |> Decode.map (\byte -> Decode.Loop ( index + 1, Array.push byte acc ))
