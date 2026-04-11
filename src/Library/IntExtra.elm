module Library.IntExtra exposing
    ( toSignedInt16, toUnsignedInt16
    , addInt16, subInt16, mulInt16, divInt16, modInt16
    , xorshift
    )

{-| Integer helpers that aren't specific to any domain.


# 16-bit conversion

@docs toSignedInt16, toUnsignedInt16


# 16-bit signed arithmetic

Operations that treat their inputs as signed 16-bit values, compute the
result with Elm's arbitrary-precision `Int`, then wrap the output back
into the unsigned 16-bit range. Matches the behavior Z-machine style
interpreters expect from add/sub/mul/div/mod.

@docs addInt16, subInt16, mulInt16, divInt16, modInt16


# Pseudo-random numbers

@docs xorshift

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


{-| Signed 16-bit addition: both operands are interpreted as signed
`Int16`, added, and wrapped back into the unsigned 16-bit range.
-}
addInt16 : Int -> Int -> Int
addInt16 =
    withSigned16 (+)


{-| Signed 16-bit subtraction.
-}
subInt16 : Int -> Int -> Int
subInt16 =
    withSigned16 (-)


{-| Signed 16-bit multiplication.
-}
mulInt16 : Int -> Int -> Int
mulInt16 =
    withSigned16 (*)


{-| Signed 16-bit truncated division (rounds toward zero).
-}
divInt16 : Int -> Int -> Int
divInt16 =
    withSigned16 truncDiv


{-| Signed 16-bit truncated modulo (the remainder that pairs with
`divInt16`).
-}
modInt16 : Int -> Int -> Int
modInt16 =
    withSigned16 truncMod


withSigned16 : (Int -> Int -> Int) -> Int -> Int -> Int
withSigned16 op a b =
    toUnsignedInt16 (op (toSignedInt16 a) (toSignedInt16 b))


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


{-| Advance a 32-bit xorshift PRNG by one step. Given a seed, returns
`( randomNumber, newSeed )` where both values are non-negative 31-bit
integers; the new seed should be threaded into the next call.

The masking to 31 bits keeps the result safely within the range that
Elm's `Int` supports as a plain number on all platforms.
-}
xorshift : Int -> ( Int, Int )
xorshift seed =
    let
        s1 =
            Bitwise.xor seed (Bitwise.shiftLeftBy 13 seed)

        s2 =
            Bitwise.xor s1 (Bitwise.shiftRightZfBy 17 s1)

        s3 =
            Bitwise.xor s2 (Bitwise.shiftLeftBy 5 s2)

        next =
            Bitwise.and s3 0x7FFFFFFF
    in
    ( next, next )
