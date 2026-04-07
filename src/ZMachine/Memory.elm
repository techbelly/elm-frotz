module ZMachine.Memory exposing
    ( Memory
    , fromBytes
    , readByte
    , readWord
    , readSignedWord
    , writeByte
    , writeWord
    , readSlice
    , size
    , dynamicSize
    , unpackAddress
    )

{-| Byte-addressable memory for the Z-Machine.

Dynamic memory (writable) is backed by an `Array Int` for O(log32 n) random access.
Static and high memory (read-only) are kept as the original `Bytes` value.

@docs Memory, fromBytes
@docs readByte, readWord, readSignedWord
@docs writeByte, writeWord
@docs readSlice, size, dynamicSize, unpackAddress

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode


{-| The Z-Machine memory image. Opaque type — use the read/write functions.
-}
type Memory
    = Memory
        { dynamic : Array Int
        , static : Bytes
        , staticBase : Int
        , fileLength : Int
        }


{-| Load a story file from raw bytes into the Memory model.

Validates that:

  - The file is at least 64 bytes (minimum header size)
  - Version byte is 3
  - Static memory base is within bounds

-}
fromBytes : Bytes -> Result String Memory
fromBytes raw =
    let
        len =
            Bytes.width raw
    in
    if len < 64 then
        Err "Story file too small (minimum 64 bytes)"

    else
        case decodeHeader raw of
            Nothing ->
                Err "Failed to decode story file header"

            Just ( version, staticBase ) ->
                if version /= 3 then
                    Err ("Unsupported Z-Machine version: " ++ String.fromInt version ++ " (only version 3 is supported)")

                else if staticBase < 64 then
                    Err ("Invalid static memory base: " ++ String.fromInt staticBase ++ " (must be >= 64)")

                else if staticBase > len then
                    Err ("Static memory base " ++ String.fromInt staticBase ++ " exceeds file length " ++ String.fromInt len)

                else
                    case decodeDynamicRegion staticBase raw of
                        Nothing ->
                            Err "Failed to read dynamic memory region"

                        Just dynamicArray ->
                            Ok
                                (Memory
                                    { dynamic = dynamicArray
                                    , static = raw
                                    , staticBase = staticBase
                                    , fileLength = len
                                    }
                                )


{-| Read a single byte (0-255) from the given address.
-}
readByte : Int -> Memory -> Int
readByte addr (Memory mem) =
    if addr < 0 then
        0

    else if addr < mem.staticBase then
        Array.get addr mem.dynamic |> Maybe.withDefault 0

    else
        decodeByteAt addr mem.static |> Maybe.withDefault 0


{-| Read an unsigned 16-bit big-endian word from the given address.
-}
readWord : Int -> Memory -> Int
readWord addr mem =
    let
        hi =
            readByte addr mem

        lo =
            readByte (addr + 1) mem
    in
    Bitwise.or (Bitwise.shiftLeftBy 8 hi) lo


{-| Read a signed 16-bit big-endian word from the given address.
Returns a value in the range -32768 to 32767.
-}
readSignedWord : Int -> Memory -> Int
readSignedWord addr mem =
    let
        unsigned =
            readWord addr mem
    in
    if unsigned > 32767 then
        unsigned - 65536

    else
        unsigned


{-| Write a single byte (0-255) to the given address.

Only dynamic memory is writable. Writes to static or high memory are silently
ignored (these indicate a bug in the calling code, not a user error).

-}
writeByte : Int -> Int -> Memory -> Memory
writeByte addr value (Memory mem) =
    if addr < 0 || addr >= mem.staticBase then
        Memory mem

    else
        Memory { mem | dynamic = Array.set addr (Bitwise.and value 0xFF) mem.dynamic }


{-| Write an unsigned 16-bit big-endian word to the given address.
-}
writeWord : Int -> Int -> Memory -> Memory
writeWord addr value mem =
    let
        hi =
            Bitwise.and (Bitwise.shiftRightZfBy 8 value) 0xFF

        lo =
            Bitwise.and value 0xFF
    in
    mem
        |> writeByte addr hi
        |> writeByte (addr + 1) lo


{-| Read a slice of bytes as a list, starting at `addr` for `count` bytes.
-}
readSlice : Int -> Int -> Memory -> List Int
readSlice addr count mem =
    List.range 0 (count - 1)
        |> List.map (\i -> readByte (addr + i) mem)


{-| Total size of the memory image in bytes.
-}
size : Memory -> Int
size (Memory mem) =
    mem.fileLength


{-| Size of the dynamic (writable) memory region in bytes.
-}
dynamicSize : Memory -> Int
dynamicSize (Memory mem) =
    mem.staticBase


{-| Convert a packed address to a byte address (V3: multiply by 2).
-}
unpackAddress : Int -> Int
unpackAddress packed =
    packed * 2



-- INTERNAL HELPERS


{-| Decode version byte and static memory base from the header.
-}
decodeHeader : Bytes -> Maybe ( Int, Int )
decodeHeader raw =
    let
        decoder =
            Decode.map2 Tuple.pair
                Decode.unsignedInt8
                (skipThenDecode 13 (Decode.unsignedInt16 Bytes.BE))
    in
    Decode.decode decoder raw


{-| Decode the dynamic memory region (bytes 0 to staticBase-1) into an Array.
-}
decodeDynamicRegion : Int -> Bytes -> Maybe (Array Int)
decodeDynamicRegion count raw =
    let
        decoder =
            Decode.loop ( 0, Array.empty ) (dynamicByteStep count)
    in
    Decode.decode decoder raw


dynamicByteStep : Int -> ( Int, Array Int ) -> Decode.Decoder (Decode.Step ( Int, Array Int ) (Array Int))
dynamicByteStep count ( index, acc ) =
    if index >= count then
        Decode.succeed (Decode.Done acc)

    else
        Decode.unsignedInt8
            |> Decode.map (\byte -> Decode.Loop ( index + 1, Array.push byte acc ))


{-| Skip `n` bytes then run a decoder.
-}
skipThenDecode : Int -> Decode.Decoder a -> Decode.Decoder a
skipThenDecode n decoder =
    Decode.bytes n
        |> Decode.andThen (\_ -> decoder)


{-| Decode a single byte at a specific offset in a Bytes value.
-}
decodeByteAt : Int -> Bytes -> Maybe Int
decodeByteAt offset raw =
    if offset < 0 || offset >= Bytes.width raw then
        Nothing

    else
        Decode.decode
            (skipThenDecode offset Decode.unsignedInt8)
            raw
