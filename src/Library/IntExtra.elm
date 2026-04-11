module Library.IntExtra exposing (toUnsignedInt16)

{-| Integer helpers that aren't specific to any domain.

@docs toUnsignedInt16

-}

import Bitwise


{-| Truncate an `Int` to its low 16 bits, interpreting the result as an
unsigned value in the range `0..65535`. Negative inputs wrap via two's
complement (e.g. `-1` becomes `65535`).
-}
toUnsignedInt16 : Int -> Int
toUnsignedInt16 n =
    Bitwise.and n 0xFFFF
