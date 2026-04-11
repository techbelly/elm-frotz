module Library.IntExtra exposing (toSignedInt16, toUnsignedInt16, truncDiv, truncMod)

{-| Integer helpers that aren't specific to any domain.

@docs toSignedInt16, toUnsignedInt16, truncDiv, truncMod

-}

import Bitwise


{-| Truncate an `Int` to its low 16 bits, interpreting the result as an
unsigned value in the range `0..65535`. Negative inputs wrap via two's
complement (e.g. `-1` becomes `65535`).
-}
toUnsignedInt16 : Int -> Int
toUnsignedInt16 n =
    Bitwise.and n 0xFFFF


{-| Interpret the low 16 bits of `n` as a signed two's-complement value
in the range `-32768..32767`.
-}
toSignedInt16 : Int -> Int
toSignedInt16 n =
    if n > 32767 then
        n - 65536

    else
        n


{-| Truncated integer division: rounds toward zero, matching C's `/`.
Elm's `//` floors toward negative infinity, which disagrees for negative
operands (e.g. `-7 // 2 == -4`, but `truncDiv -7 2 == -3`).
-}
truncDiv : Int -> Int -> Int
truncDiv a b =
    let
        result =
            toFloat a / toFloat b
    in
    if result < 0 then
        ceiling result

    else
        floor result


{-| Truncated modulo: the remainder that pairs with `truncDiv`, so
`truncDiv a b * b + truncMod a b == a`. Differs from Elm's `modBy` for
negative operands.
-}
truncMod : Int -> Int -> Int
truncMod a b =
    a - truncDiv a b * b
