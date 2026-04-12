module Library.CMem exposing (compress, decompress)

{-| XOR-and-run-length-encoded memory diffs.

This implements the CMem compression scheme from the Quetzal save-file
specification (§3.2): XOR each byte of the current dynamic memory
against the original, then run-length encode the resulting zeros.

The format is compact for typical save states where only a small
fraction of dynamic memory has changed. It is used by the native
snapshot codec and will be reused by the Quetzal encoder.


# Encoding

@docs compress


# Decoding

@docs decompress

-}

import Array exposing (Array)
import Bitwise


{-| XOR `current` against `original` and run-length encode the zeros.

Non-zero XOR bytes are emitted as-is. A run of N consecutive zero bytes
is encoded as `0x00` followed by a single byte `N − 1`, so each pair
covers 1–256 zeros. Trailing zeros are omitted entirely (the
decompressor pads to the original length).

Both arrays must be the same length (the dynamic memory size). If
`current` is shorter the missing bytes are treated as zero; if longer
the excess is ignored.

-}
compress : { original : Array Int, current : Array Int } -> List Int
compress { original, current } =
    let
        len =
            Array.length original
    in
    compressHelp original current 0 len 0 []
        |> List.reverse


{-| Walk the arrays, XOR each pair, and accumulate the compressed output.

`zeroRun` tracks how many consecutive zero-XOR bytes we've seen but not
yet flushed. Non-zero bytes flush any pending zero run first.

-}
compressHelp : Array Int -> Array Int -> Int -> Int -> Int -> List Int -> List Int
compressHelp original current i len zeroRun acc =
    if i >= len then
        -- Trailing zeros are omitted per Quetzal spec.
        acc

    else
        let
            orig =
                Array.get i original |> Maybe.withDefault 0

            cur =
                Array.get i current |> Maybe.withDefault 0

            xor =
                Bitwise.xor orig cur |> Bitwise.and 0xFF
        in
        if xor == 0 then
            -- Extend the zero run. If it hits 256, flush one pair.
            if zeroRun >= 255 then
                compressHelp original current (i + 1) len 0 (255 :: 0 :: acc)

            else
                compressHelp original current (i + 1) len (zeroRun + 1) acc

        else
            -- Flush any pending zero run, then emit the non-zero byte.
            let
                flushed =
                    if zeroRun > 0 then
                        xor :: (zeroRun - 1) :: 0 :: acc

                    else
                        xor :: acc
            in
            compressHelp original current (i + 1) len 0 flushed


{-| Decompress a CMem byte list against the original dynamic memory,
producing the reconstructed current dynamic memory.

The compressed data is XOR-and-RLE encoded (see [`compress`](#compress)).
The decompressor walks the compressed stream, XORing non-zero bytes
against the original at the corresponding position. Any bytes beyond
the end of the compressed stream are taken from the original unchanged
(implicit trailing zeros).

Returns `Err` if the compressed data would produce more bytes than the
original dynamic memory length.

-}
decompress : { compressed : List Int, original : Array Int } -> Result String (Array Int)
decompress { compressed, original } =
    let
        len =
            Array.length original
    in
    decompressHelp compressed original 0 len original
        |> Result.map identity


decompressHelp : List Int -> Array Int -> Int -> Int -> Array Int -> Result String (Array Int)
decompressHelp bytes original i len acc =
    case bytes of
        [] ->
            -- Remaining positions are implicitly zero XOR → original unchanged.
            Ok acc

        0 :: rest ->
            -- Zero byte followed by run-length count.
            case rest of
                countByte :: tail ->
                    let
                        runLen =
                            countByte + 1

                        nextI =
                            i + runLen
                    in
                    if nextI > len then
                        Err "CMem decompression overflows dynamic memory"

                    else
                        -- All zeros XOR original = original, so acc is unchanged.
                        decompressHelp tail original nextI len acc

                [] ->
                    -- Trailing 0x00 with no count byte — treat as a single zero.
                    -- (Shouldn't happen from a well-formed encoder, but be lenient.)
                    if i + 1 > len then
                        Err "CMem decompression overflows dynamic memory"

                    else
                        Ok acc

        xorByte :: rest ->
            if i >= len then
                Err "CMem decompression overflows dynamic memory"

            else
                let
                    orig =
                        Array.get i original |> Maybe.withDefault 0

                    restored =
                        Bitwise.xor orig xorByte |> Bitwise.and 0xFF
                in
                decompressHelp rest original (i + 1) len (Array.set i restored acc)
