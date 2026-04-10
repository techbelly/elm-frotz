module ZMachine.Header exposing
    ( version, highMemoryBase, initialPC
    , dictionaryAddress, objectTableAddress, globalVariablesAddress
    , staticMemoryBase, abbreviationsTableAddress
    , fileLength, checksum, releaseNumber, serialNumber, standardRevision
    , Flag1(..), Flag2(..), testFlag1, testFlag2, setFlag2, clearFlag2
    , setInterpreterInfo, setScreenSize, setStandardRevision
    )

{-| Accessors for the 64-byte Z-Machine header.

All reads go through `ZMachine.Memory.readByte`/`readWord` so they always
reflect the current state of dynamic memory (the header lives in dynamic memory
and some fields are writable).

@docs version, highMemoryBase, initialPC
@docs dictionaryAddress, objectTableAddress, globalVariablesAddress
@docs staticMemoryBase, abbreviationsTableAddress
@docs fileLength, checksum, releaseNumber, serialNumber, standardRevision
@docs Flag1, Flag2, testFlag1, testFlag2, setFlag2, clearFlag2
@docs setInterpreterInfo, setScreenSize, setStandardRevision

-}

import Bitwise
import ZMachine.Memory as Memory exposing (Memory)


{-| Flags in header byte 0x01 (Flags 1) relevant to V3.

  - `StatusLineType` (bit 1): 0 = score/turns, 1 = hours:minutes
  - `StoryFileSplit` (bit 2): story file split across two discs
  - `StatusLineNotAvailable` (bit 4): interpreter sets if no status line
  - `ScreenSplitAvailable` (bit 5): interpreter sets if upper window supported
  - `VariablePitchDefault` (bit 6): variable-pitch font is default

-}
type Flag1
    = StatusLineType
    | StoryFileSplit
    | StatusLineNotAvailable
    | ScreenSplitAvailable
    | VariablePitchDefault


{-| Flags in header byte 0x10 (Flags 2) relevant to V3.

  - `Transcripting` (bit 0): transcript output stream is active
  - `ForceFixedPitch` (bit 1): game requests fixed-pitch font

-}
type Flag2
    = Transcripting
    | ForceFixedPitch



-- SIMPLE FIELD READS


{-| Version number (byte 0x00). Should be 3 for .z3 files.
-}
version : Memory -> Int
version mem =
    Memory.readByte 0x00 mem


{-| Flags 1 raw byte (byte 0x01).
-}
flags1 : Memory -> Int
flags1 mem =
    Memory.readByte 0x01 mem


{-| Base of high memory (word at 0x04).
-}
highMemoryBase : Memory -> Int
highMemoryBase mem =
    Memory.readWord 0x04 mem


{-| Initial program counter (word at 0x06).
In V3 this is the byte address of the first instruction to execute.
-}
initialPC : Memory -> Int
initialPC mem =
    Memory.readWord 0x06 mem


{-| Dictionary table byte address (word at 0x08).
-}
dictionaryAddress : Memory -> Int
dictionaryAddress mem =
    Memory.readWord 0x08 mem


{-| Object table byte address (word at 0x0A).
-}
objectTableAddress : Memory -> Int
objectTableAddress mem =
    Memory.readWord 0x0A mem


{-| Global variables table byte address (word at 0x0C).
-}
globalVariablesAddress : Memory -> Int
globalVariablesAddress mem =
    Memory.readWord 0x0C mem


{-| Static memory base byte address (word at 0x0E).
-}
staticMemoryBase : Memory -> Int
staticMemoryBase mem =
    Memory.readWord 0x0E mem


{-| Flags 2 raw byte (byte 0x10).
-}
flags2 : Memory -> Int
flags2 mem =
    Memory.readByte 0x10 mem


{-| Abbreviations table byte address (word at 0x18).
-}
abbreviationsTableAddress : Memory -> Int
abbreviationsTableAddress mem =
    Memory.readWord 0x18 mem


{-| Release number (word at 0x02).
-}
releaseNumber : Memory -> Int
releaseNumber mem =
    Memory.readWord 0x02 mem


{-| Serial number — 6 ASCII bytes at 0x12-0x17.
Usually a date in the form YYMMDD.
-}
serialNumber : Memory -> String
serialNumber mem =
    Memory.readSlice 0x12 6 mem
        |> List.map Char.fromCode
        |> String.fromList


{-| File length (word at 0x1A). In V3, multiply by 2 to get actual byte count.
Returns 0 for early V3 files that don't include this field.
-}
fileLength : Memory -> Int
fileLength mem =
    Memory.readWord 0x1A mem * 2


{-| File checksum (word at 0x1C). Sum of all bytes from 0x40 onward, mod 65536.
Returns 0 for early V3 files that don't include this field.
-}
checksum : Memory -> Int
checksum mem =
    Memory.readWord 0x1C mem


{-| Standard revision number (bytes 0x32-0x33). Major.Minor.
Returns (0, 0) if not set.
-}
standardRevision : Memory -> ( Int, Int )
standardRevision mem =
    ( Memory.readByte 0x32 mem, Memory.readByte 0x33 mem )



-- FLAG TESTING AND SETTING


{-| Test whether a Flags 1 bit is set.
-}
testFlag1 : Flag1 -> Memory -> Bool
testFlag1 flag mem =
    Bitwise.and (flags1 mem) (flag1Bit flag) /= 0


{-| Test whether a Flags 2 bit is set.
-}
testFlag2 : Flag2 -> Memory -> Bool
testFlag2 flag mem =
    Bitwise.and (flags2 mem) (flag2Bit flag) /= 0


{-| Set a Flags 2 bit. (Only Flags 2 contains interpreter-writable bits in V3.)
-}
setFlag2 : Flag2 -> Memory -> Memory
setFlag2 flag mem =
    let
        current =
            flags2 mem
    in
    Memory.writeByte 0x10 (Bitwise.or current (flag2Bit flag)) mem


{-| Clear a Flags 2 bit.
-}
clearFlag2 : Flag2 -> Memory -> Memory
clearFlag2 flag mem =
    let
        current =
            flags2 mem
    in
    Memory.writeByte 0x10 (Bitwise.and current (Bitwise.complement (flag2Bit flag))) mem



-- INTERPRETER-SET FIELDS


{-| Write interpreter number and version into the header.
Called during initialization so the game can query what interpreter it's running on.

Common interpreter numbers:

  - 1 = DECSystem-20, 2 = Apple IIe, 3 = Macintosh, 4 = Amiga
  - 5 = Atari ST, 6 = IBM PC, 7 = Commodore 128, 8 = C64
  - 9 = Apple IIc, 10 = Apple IIgs, 11 = Tandy Color

-}
setInterpreterInfo : Int -> Int -> Memory -> Memory
setInterpreterInfo number versionChar mem =
    mem
        |> Memory.writeByte 0x1E number
        |> Memory.writeByte 0x1F versionChar


{-| Write screen height (lines) and width (characters) into the header.
-}
setScreenSize : Int -> Int -> Memory -> Memory
setScreenSize height width mem =
    mem
        |> Memory.writeByte 0x20 height
        |> Memory.writeByte 0x21 width


{-| Write the standard revision number this interpreter conforms to.
-}
setStandardRevision : Int -> Int -> Memory -> Memory
setStandardRevision major minor mem =
    mem
        |> Memory.writeByte 0x32 major
        |> Memory.writeByte 0x33 minor



-- INTERNAL


flag1Bit : Flag1 -> Int
flag1Bit flag =
    case flag of
        StatusLineType ->
            Bitwise.shiftLeftBy 1 1

        StoryFileSplit ->
            Bitwise.shiftLeftBy 2 1

        StatusLineNotAvailable ->
            Bitwise.shiftLeftBy 4 1

        ScreenSplitAvailable ->
            Bitwise.shiftLeftBy 5 1

        VariablePitchDefault ->
            Bitwise.shiftLeftBy 6 1


flag2Bit : Flag2 -> Int
flag2Bit flag =
    case flag of
        Transcripting ->
            1

        ForceFixedPitch ->
            Bitwise.shiftLeftBy 1 1
