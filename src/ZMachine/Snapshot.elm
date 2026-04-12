module ZMachine.Snapshot exposing
    ( Snapshot
    , SnapshotMeta
    , ResumeKind(..)
    , RestoreError(..)
    , capture
    , restore
    , meta
    , resumeKind
    , pc
    , dynamicMemory
    , stack
    , callStack
    , fromParts
    , encode
    , decode
    )

{-| Save/restore snapshots for the Z-Machine.

A `Snapshot` captures the mutable state of a running Z-Machine so that
it can be serialized to bytes and later fed back to restore execution.
There are two ways to produce one:

1.  The host calls [`ZMachine.snapshot`](ZMachine#snapshot) at any point
    between steps — produces an **autosave** snapshot tagged `ResumeAt`.
2.  The interpreter produces one automatically when a running story
    executes the `save` opcode — tagged `ResumeByBranchTrue`, meaning
    that on restore the save instruction's branch-on-success is taken.

Clients that want a standard format should use
[`ZMachine.Quetzal`](ZMachine-Quetzal) for `ResumeByBranchTrue`
snapshots and [`encode`](#encode) for autosaves. Clients building
custom schemes (transcript + notes, etc.) can use the projection
functions and reconstruct a `Snapshot` via [`fromParts`](#fromParts).

@docs Snapshot, SnapshotMeta, ResumeKind, RestoreError


# Capture and restore

@docs capture, restore


# Inspection

@docs meta, resumeKind, pc, dynamicMemory, stack, callStack


# Custom schemes

@docs fromParts


# Native binary format

@docs encode, decode

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import Library.CMem as CMem
import ZMachine.Header as Header
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Opcode exposing (VariableRef(..))
import ZMachine.Stack exposing (CallFrame)


{-| Opaque snapshot of Z-Machine state.
-}
type Snapshot
    = Snapshot
        { dynamicMem : Array Int
        , pcAddr : Int
        , evalStack : List Int
        , callFrames : List CallFrame
        , resume : ResumeKind
        , metaData : SnapshotMeta
        }


{-| Story-identifying metadata stored inside a snapshot. Used to reject
snapshots that belong to a different story or a different release.
-}
type alias SnapshotMeta =
    { release : Int
    , serial : String
    , checksum : Int
    }


{-| How the interpreter should resume after restoring this snapshot.

  - `ResumeAt` — host-initiated snapshot (autosave). Execution continues
    from `pc` as if nothing happened.
  - `ResumeByBranchTrue` — produced by the `save` opcode. Execution
    re-runs the save instruction at `pc` with a success result,
    following its branch-on-true target.

-}
type ResumeKind
    = ResumeAt
    | ResumeByBranchTrue


{-| Errors returned by [`restore`](#restore).

  - `WrongStory` — snapshot metadata does not match the currently-loaded
    story. Compare `expected` (current story) against `found` (snapshot).
  - `SnapshotCorrupt` — snapshot data failed an integrity check.

-}
type RestoreError
    = WrongStory { expected : SnapshotMeta, found : SnapshotMeta }
    | SnapshotCorrupt String



-- CAPTURE / RESTORE


{-| Build a snapshot from the live pieces of a running Z-Machine.
Metadata is read from the supplied memory image's header.
-}
capture :
    { memory : Memory
    , pc : Int
    , stack : List Int
    , callStack : List CallFrame
    , resumeKind : ResumeKind
    }
    -> Snapshot
capture args =
    Snapshot
        { dynamicMem = Memory.dynamicBytes args.memory
        , pcAddr = args.pc
        , evalStack = args.stack
        , callFrames = args.callStack
        , resume = args.resumeKind
        , metaData =
            { release = Header.releaseNumber args.memory
            , serial = Header.serialNumber args.memory
            , checksum = Header.checksum args.memory
            }
        }


{-| Validate a snapshot against the currently-loaded story's memory and
produce the pieces needed to rehydrate a running machine.

The supplied `Memory` must be the **original** image of the currently
loaded story (i.e. `ZMachine.originalMemory`), so that metadata can be
compared and the static/high memory region can be reused when rebuilding
the dynamic memory image.

-}
restore : Snapshot -> Memory -> Result RestoreError { memory : Memory, pc : Int, stack : List Int, callStack : List CallFrame }
restore (Snapshot snap) originalMemory =
    let
        currentMeta =
            { release = Header.releaseNumber originalMemory
            , serial = Header.serialNumber originalMemory
            , checksum = Header.checksum originalMemory
            }
    in
    if snap.metaData /= currentMeta then
        Err (WrongStory { expected = currentMeta, found = snap.metaData })

    else
        Memory.replaceDynamic snap.dynamicMem originalMemory
            |> Result.mapError SnapshotCorrupt
            |> Result.map
                (\rebuiltMemory ->
                    { memory = rebuiltMemory
                    , pc = snap.pcAddr
                    , stack = snap.evalStack
                    , callStack = snap.callFrames
                    }
                )



-- INSPECTION


{-| Story metadata recorded in the snapshot.
-}
meta : Snapshot -> SnapshotMeta
meta (Snapshot snap) =
    snap.metaData


{-| Which resume policy this snapshot was tagged with.
-}
resumeKind : Snapshot -> ResumeKind
resumeKind (Snapshot snap) =
    snap.resume


{-| The program counter stored in the snapshot. For a `ResumeAt`
snapshot this is the next instruction to execute; for
`ResumeByBranchTrue` it points at the save opcode whose branch will be
re-evaluated as true.
-}
pc : Snapshot -> Int
pc (Snapshot snap) =
    snap.pcAddr


{-| Raw dynamic memory bytes captured in the snapshot. Length equals the
dynamic memory size of the story (i.e. the header's static base).
-}
dynamicMemory : Snapshot -> Array Int
dynamicMemory (Snapshot snap) =
    snap.dynamicMem


{-| The interpreter's current evaluation stack at capture time.
-}
stack : Snapshot -> List Int
stack (Snapshot snap) =
    snap.evalStack


{-| The active call frames at capture time, outermost-first.
-}
callStack : Snapshot -> List CallFrame
callStack (Snapshot snap) =
    snap.callFrames



-- CUSTOM SCHEMES


{-| Build a `Snapshot` directly from its constituent parts. Useful for
clients implementing custom save formats: after decoding your format,
call this to produce a `Snapshot` that can be fed to
[`restore`](#restore).
-}
fromParts :
    { dynamicMemory : Array Int
    , pc : Int
    , stack : List Int
    , callStack : List CallFrame
    , resumeKind : ResumeKind
    , meta : SnapshotMeta
    }
    -> Snapshot
fromParts parts =
    Snapshot
        { dynamicMem = parts.dynamicMemory
        , pcAddr = parts.pc
        , evalStack = parts.stack
        , callFrames = parts.callStack
        , resume = parts.resumeKind
        , metaData = parts.meta
        }



-- NATIVE BINARY FORMAT
--
-- Purpose: a simple self-describing serialization for clients that want
-- to persist snapshots (especially autosaves) without writing their own
-- byte code. Not cross-interpreter portable — use ZMachine.Quetzal for
-- that. Layout:
--
--   magic         4 bytes   "ELZS"
--   version       1 byte    format version (= 1)
--   resumeKind    1 byte    0 = ResumeAt, 1 = ResumeByBranchTrue
--   release       2 bytes   big-endian unsigned
--   serial        6 bytes   ASCII
--   checksum      2 bytes   big-endian unsigned
--   pc            4 bytes   big-endian unsigned
--   dynMemLen     4 bytes   big-endian unsigned
--   dynMem        dynMemLen bytes
--   stackLen      4 bytes   big-endian unsigned
--   stack         stackLen * 2 bytes (each word big-endian)
--   frameCount    4 bytes   big-endian unsigned
--   frames...
--
-- Each frame:
--   returnPC      4 bytes
--   storeFlag     1 byte   (0 = no return store, 1 = present)
--   storeKind     1 byte   (0 = Stack, 1 = Local, 2 = Global)
--   storeNum      1 byte   (index for Local/Global, ignored for Stack)
--   localsLen     1 byte
--   locals        localsLen * 2 bytes
--   evalLen       4 bytes
--   evalStack     evalLen * 2 bytes


nativeMagic : Int
nativeMagic =
    -- "ELZS" as a 32-bit big-endian word (0x454C5A53)
    0x454C5A53


nativeVersion : Int
nativeVersion =
    2


{-| Encode a snapshot as bytes using the interpreter's native format.

The `Memory` argument is the **original** story memory — it is used to
produce a compressed diff (XOR + run-length encoding) of dynamic memory
rather than storing the full image. This typically reduces the dynamic
memory payload from ~16 KB to a few hundred bytes.

Useful for storing autosaves in `localStorage`, a file, etc. The output
is not portable between different Z-Machine interpreters — use
[`ZMachine.Quetzal.encode`](ZMachine-Quetzal#encode) for interop.

-}
encode : Memory -> Snapshot -> Bytes
encode originalMemory (Snapshot snap) =
    let
        compressedDynMem =
            CMem.compress
                { original = Memory.dynamicBytes originalMemory
                , current = snap.dynamicMem
                }
    in
    BE.sequence
        [ BE.unsignedInt32 Bytes.BE nativeMagic
        , BE.unsignedInt8 nativeVersion
        , BE.unsignedInt8 (resumeKindToByte snap.resume)
        , BE.unsignedInt16 Bytes.BE snap.metaData.release
        , encodeSerial snap.metaData.serial
        , BE.unsignedInt16 Bytes.BE snap.metaData.checksum
        , BE.unsignedInt32 Bytes.BE snap.pcAddr
        , encodeLengthPrefixed BE.unsignedInt8 compressedDynMem
        , encodeLengthPrefixed encodeWord snap.evalStack
        , encodeLengthPrefixed encodeFrame snap.callFrames
        ]
        |> BE.encode


{-| Decode a snapshot from bytes produced by [`encode`](#encode).

The `Memory` argument is the **original** story memory — it is needed
to decompress the dynamic memory diff stored in the snapshot.

Returns `Err` with a reason string on any format error.

-}
decode : Memory -> Bytes -> Result String Snapshot
decode originalMemory bytes =
    BD.decode (nativeDecoder originalMemory) bytes
        |> Maybe.withDefault (Err "Snapshot bytes truncated or malformed")



-- NATIVE ENCODE HELPERS


resumeKindToByte : ResumeKind -> Int
resumeKindToByte kind =
    case kind of
        ResumeAt ->
            0

        ResumeByBranchTrue ->
            1


encodeSerial : String -> BE.Encoder
encodeSerial s =
    let
        padded =
            (s ++ "      ") |> String.left 6

        codes =
            String.toList padded |> List.map Char.toCode
    in
    BE.sequence (List.map BE.unsignedInt8 codes)


{-| Encode a u32 big-endian length prefix followed by `items`, each
encoded via `itemEncoder`. Inverse of [`lengthPrefixed`](#lengthPrefixed).
-}
encodeLengthPrefixed : (a -> BE.Encoder) -> List a -> BE.Encoder
encodeLengthPrefixed itemEncoder items =
    BE.sequence
        (BE.unsignedInt32 Bytes.BE (List.length items)
            :: List.map itemEncoder items
        )


{-| Encode a single 16-bit word, masking to the low 16 bits so negative
Ints (e.g. signed stack values) round-trip through an unsigned decoder.
-}
encodeWord : Int -> BE.Encoder
encodeWord w =
    BE.unsignedInt16 Bytes.BE (Bitwise.and w 0xFFFF)


encodeStoreRef : Maybe VariableRef -> BE.Encoder
encodeStoreRef ref =
    let
        ( flag, kind, num ) =
            case ref of
                Nothing ->
                    ( 0, 0, 0 )

                Just Stack ->
                    ( 1, 0, 0 )

                Just (Local n) ->
                    ( 1, 1, n )

                Just (Global n) ->
                    ( 1, 2, n )
    in
    BE.sequence
        [ BE.unsignedInt8 flag
        , BE.unsignedInt8 kind
        , BE.unsignedInt8 num
        ]


encodeFrame : CallFrame -> BE.Encoder
encodeFrame frame =
    BE.sequence
        [ BE.unsignedInt32 Bytes.BE frame.returnPC
        , encodeStoreRef frame.returnStore
        , BE.unsignedInt8 (Array.length frame.locals)
        , BE.sequence (Array.toList frame.locals |> List.map encodeWord)
        , encodeLengthPrefixed encodeWord frame.evalStack
        ]



-- NATIVE DECODE HELPERS


nativeDecoder : Memory -> BD.Decoder (Result String Snapshot)
nativeDecoder originalMemory =
    BD.map2 Tuple.pair
        (BD.unsignedInt32 Bytes.BE)
        BD.unsignedInt8
        |> BD.andThen
            (\( magic, version ) ->
                if magic /= nativeMagic then
                    BD.succeed (Err "Snapshot bytes have wrong magic")

                else if version /= nativeVersion then
                    BD.succeed (Err ("Snapshot format version " ++ String.fromInt version ++ " not supported"))

                else
                    decodeBody (Memory.dynamicBytes originalMemory)
            )


decodeBody : Array Int -> BD.Decoder (Result String Snapshot)
decodeBody originalDynamic =
    BD.succeed (assembleSnapshot originalDynamic)
        |> andMap BD.unsignedInt8
        |> andMap (BD.unsignedInt16 Bytes.BE)
        |> andMap (decodeSerial 6)
        |> andMap (BD.unsignedInt16 Bytes.BE)
        |> andMap (BD.unsignedInt32 Bytes.BE)
        |> andMap decodeByteList
        |> andMap decodeWordList
        |> andMap decodeFrames


assembleSnapshot : Array Int -> Int -> Int -> String -> Int -> Int -> List Int -> List Int -> List CallFrame -> Result String Snapshot
assembleSnapshot originalDynamic resumeByte release serial checksum pcAddr compressedDynMem evalStack frames =
    Result.map2
        (\resume dynMem ->
            Snapshot
                { dynamicMem = dynMem
                , pcAddr = pcAddr
                , evalStack = evalStack
                , callFrames = frames
                , resume = resume
                , metaData =
                    { release = release
                    , serial = serial
                    , checksum = checksum
                    }
                }
        )
        (byteToResumeKind resumeByte)
        (CMem.decompress
            { compressed = compressedDynMem
            , original = originalDynamic
            }
        )


{-| Applicative apply for `BD.Decoder`. Lets us build a decoder that
reads N fields in sequence and feeds them to a curried N-argument
function without nesting `andThen`s (or running out of `mapN`s).

Reads the function-decoder first, then the argument-decoder — matching
the pipeline ordering `BD.succeed f |> andMap a |> andMap b`.

-}
andMap : BD.Decoder a -> BD.Decoder (a -> b) -> BD.Decoder b
andMap decA decFn =
    BD.map2 (\f a -> f a) decFn decA


byteToResumeKind : Int -> Result String ResumeKind
byteToResumeKind b =
    case b of
        0 ->
            Ok ResumeAt

        1 ->
            Ok ResumeByBranchTrue

        _ ->
            Err ("Unknown resume kind byte " ++ String.fromInt b)


decodeSerial : Int -> BD.Decoder String
decodeSerial n =
    repeatDecoder n BD.unsignedInt8
        |> BD.map (List.map Char.fromCode >> String.fromList)


decodeByteList : BD.Decoder (List Int)
decodeByteList =
    lengthPrefixed BD.unsignedInt8


decodeWordList : BD.Decoder (List Int)
decodeWordList =
    lengthPrefixed (BD.unsignedInt16 Bytes.BE)


decodeFrames : BD.Decoder (List CallFrame)
decodeFrames =
    lengthPrefixed decodeFrame


decodeFrame : BD.Decoder CallFrame
decodeFrame =
    BD.map3 (\rpc sr ll -> ( rpc, sr, ll ))
        (BD.unsignedInt32 Bytes.BE)
        decodeStoreRef
        BD.unsignedInt8
        |> BD.andThen decodeFrameBody


decodeFrameBody : ( Int, Maybe VariableRef, Int ) -> BD.Decoder CallFrame
decodeFrameBody ( returnPC, storeRef, localsLen ) =
    BD.map2
        (\locals evalStack ->
            { returnPC = returnPC
            , returnStore = storeRef
            , locals = locals
            , evalStack = evalStack
            }
        )
        (repeatDecoder localsLen (BD.unsignedInt16 Bytes.BE)
            |> BD.map Array.fromList
        )
        decodeWordList


decodeStoreRef : BD.Decoder (Maybe VariableRef)
decodeStoreRef =
    BD.map3 storeRefFromBytes
        BD.unsignedInt8
        BD.unsignedInt8
        BD.unsignedInt8


storeRefFromBytes : Int -> Int -> Int -> Maybe VariableRef
storeRefFromBytes flag kind num =
    if flag == 0 then
        Nothing

    else
        case kind of
            0 ->
                Just Stack

            1 ->
                Just (Local num)

            _ ->
                Just (Global num)


{-| Read a u32 big-endian length, then that many items. Inverse of
[`encodeLengthPrefixed`](#encodeLengthPrefixed).
-}
lengthPrefixed : BD.Decoder a -> BD.Decoder (List a)
lengthPrefixed itemDecoder =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen (\count -> repeatDecoder count itemDecoder)


{-| Run the given decoder `count` times, collecting the results in order.
-}
repeatDecoder : Int -> BD.Decoder a -> BD.Decoder (List a)
repeatDecoder count itemDecoder =
    BD.loop ( count, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (List.reverse acc))

            else
                itemDecoder
                    |> BD.map (\item -> BD.Loop ( remaining - 1, item :: acc ))
        )
