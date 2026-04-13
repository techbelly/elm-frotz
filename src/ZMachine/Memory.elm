module ZMachine.Memory exposing
    ( Memory
    , Version(..)
    , VersionProfile
    , fromBytes
    , profile
    , readByte
    , readWord
    , readSignedWord
    , writeByte
    , writeWord
    , readSlice
    , size
    , dynamicSize
    , unpackAddress
    , wordLength
    , replaceDynamic
    , dynamicBytes
    )

{-| Byte-addressable memory for the Z-Machine.

The full memory image is held in a single `Array Int` for O(log32 n)
random access reads. Writes are guarded so only dynamic memory (below
`staticBase`) is mutable; writes to static/high memory are silently
ignored, matching the spec requirement that they're illegal.

@docs Memory, Version, VersionProfile, fromBytes, profile
@docs readByte, readWord, readSignedWord
@docs writeByte, writeWord
@docs readSlice, size, dynamicSize, unpackAddress, wordLength
@docs replaceDynamic, dynamicBytes

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Library.ArrayExtra as ArrayExtra
import Library.BytesExtra as BytesExtra


{-| Z-Machine version.
-}
type Version
    = V3
    | V5


{-| Version-dependent constants, computed once at load time.

Every numeric difference between Z-Machine versions lives here so the
rest of the codebase can read fields instead of switching on `Version`.
-}
type alias VersionProfile =
    { version : Version
    , packingShift : Int
    , numPropertyDefaults : Int
    , objectEntrySize : Int
    , numAttributeBytes : Int
    , propertyNumberMask : Int
    , objectPointerSize : Int
    , parentOffset : Int
    , siblingOffset : Int
    , childOffset : Int
    , propPtrOffset : Int
    , dictWordZChars : Int
    , dictWordWords : Int
    , fileLengthMultiplier : Int
    , routineHasInitialValues : Bool
    , textBufferOffset : Int
    , hasStatusLine : Bool
    }


{-| The Z-Machine memory image. Opaque type — use the read/write functions.
-}
type Memory
    = Memory
        { bytes : Array Int
        , staticBase : Int
        , fileLength : Int
        , versionProfile : VersionProfile
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

            Just ( versionByte, staticBase ) ->
                case versionFromInt versionByte of
                    Nothing ->
                        Err ("Unsupported Z-Machine version: " ++ String.fromInt versionByte ++ " (only versions 3 and 5 are supported)")

                    Just ver ->
                        if staticBase < 64 then
                            Err ("Invalid static memory base: " ++ String.fromInt staticBase ++ " (must be >= 64)")

                        else if staticBase > len then
                            Err ("Static memory base " ++ String.fromInt staticBase ++ " exceeds file length " ++ String.fromInt len)

                        else
                            Ok
                                (Memory
                                    { bytes = BytesExtra.toIntArray raw
                                    , staticBase = staticBase
                                    , fileLength = len
                                    , versionProfile = makeProfile ver
                                    }
                                )


{-| Read a single byte (0-255) from the given address.
-}
readByte : Int -> Memory -> Int
readByte addr (Memory mem) =
    if addr < 0 then
        0

    else
        Array.get addr mem.bytes |> Maybe.withDefault 0


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
        Memory { mem | bytes = Array.set addr (Bitwise.and value 0xFF) mem.bytes }


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


{-| Size of a Z-Machine word, in bytes. Words are 16-bit big-endian, so
this is always `2`. Use this instead of a bare `2` anywhere the intent
is "advance by one word" or "index the Nth word".
-}
wordLength : Int
wordLength =
    2


{-| The version profile for this memory image.
-}
profile : Memory -> VersionProfile
profile (Memory mem) =
    mem.versionProfile


{-| Convert a packed address to a byte address.
V3 shifts left by 1 (×2), V5 shifts left by 2 (×4).
-}
unpackAddress : Int -> Memory -> Int
unpackAddress packed (Memory mem) =
    Bitwise.shiftLeftBy mem.versionProfile.packingShift packed


{-| Extract a snapshot of the dynamic memory region as an `Array Int`.
Used by the save/restore machinery to capture state that can later be
fed back to [`replaceDynamic`](#replaceDynamic).
-}
dynamicBytes : Memory -> Array Int
dynamicBytes (Memory mem) =
    Array.slice 0 mem.staticBase mem.bytes


{-| Replace the dynamic memory region (bytes 0..staticBase-1) with the
given array. Fails if the array length doesn't match the dynamic memory
size of the current story. The static/high memory region is left
untouched, as is the `staticBase`/`fileLength` metadata.
-}
replaceDynamic : Array Int -> Memory -> Result String Memory
replaceDynamic newDynamic (Memory mem) =
    let
        expected =
            mem.staticBase

        actual =
            Array.length newDynamic
    in
    if actual /= expected then
        Err
            ("replaceDynamic: expected "
                ++ String.fromInt expected
                ++ " bytes, got "
                ++ String.fromInt actual
            )

    else
        Ok (Memory { mem | bytes = ArrayExtra.merge newDynamic mem.bytes })



-- INTERNAL HELPERS


versionFromInt : Int -> Maybe Version
versionFromInt n =
    case n of
        3 ->
            Just V3

        5 ->
            Just V5

        _ ->
            Nothing


makeProfile : Version -> VersionProfile
makeProfile ver =
    case ver of
        V3 ->
            { version = V3
            , packingShift = 1
            , numPropertyDefaults = 31
            , objectEntrySize = 9
            , numAttributeBytes = 4
            , propertyNumberMask = 0x1F
            , objectPointerSize = 1
            , parentOffset = 4
            , siblingOffset = 5
            , childOffset = 6
            , propPtrOffset = 7
            , dictWordZChars = 6
            , dictWordWords = 2
            , fileLengthMultiplier = 2
            , routineHasInitialValues = True
            , textBufferOffset = 1
            , hasStatusLine = True
            }

        V5 ->
            { version = V5
            , packingShift = 2
            , numPropertyDefaults = 63
            , objectEntrySize = 14
            , numAttributeBytes = 6
            , propertyNumberMask = 0x3F
            , objectPointerSize = 2
            , parentOffset = 6
            , siblingOffset = 8
            , childOffset = 10
            , propPtrOffset = 12
            , dictWordZChars = 9
            , dictWordWords = 3
            , fileLengthMultiplier = 4
            , routineHasInitialValues = False
            , textBufferOffset = 2
            , hasStatusLine = False
            }


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


{-| Skip `n` bytes then run a decoder.
-}
skipThenDecode : Int -> Decode.Decoder a -> Decode.Decoder a
skipThenDecode n decoder =
    Decode.bytes n
        |> Decode.andThen (\_ -> decoder)
