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
        case Memory.replaceDynamic snap.dynamicMem originalMemory of
            Err reason ->
                Err (SnapshotCorrupt reason)

            Ok rebuiltMemory ->
                Ok
                    { memory = rebuiltMemory
                    , pc = snap.pcAddr
                    , stack = snap.evalStack
                    , callStack = snap.callFrames
                    }



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
    1


{-| Encode a snapshot as bytes using the interpreter's native format.

Useful for storing autosaves in `localStorage`, a file, etc. The output
is not portable between different Z-Machine interpreters — use
[`ZMachine.Quetzal.encode`](ZMachine-Quetzal#encode) for interop.

-}
encode : Snapshot -> Bytes
encode (Snapshot snap) =
    BE.sequence
        [ BE.unsignedInt32 Bytes.BE nativeMagic
        , BE.unsignedInt8 nativeVersion
        , BE.unsignedInt8 (resumeKindToByte snap.resume)
        , BE.unsignedInt16 Bytes.BE snap.metaData.release
        , encodeSerial snap.metaData.serial
        , BE.unsignedInt16 Bytes.BE snap.metaData.checksum
        , BE.unsignedInt32 Bytes.BE snap.pcAddr
        , encodeByteArray snap.dynamicMem
        , encodeWordList snap.evalStack
        , encodeFrames snap.callFrames
        ]
        |> BE.encode


{-| Decode a snapshot from bytes produced by [`encode`](#encode).
Returns `Err` with a reason string on any format error.
-}
decode : Bytes -> Result String Snapshot
decode bytes =
    case BD.decode nativeDecoder bytes of
        Just (Ok snap) ->
            Ok snap

        Just (Err reason) ->
            Err reason

        Nothing ->
            Err "Snapshot bytes truncated or malformed"



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


encodeByteArray : Array Int -> BE.Encoder
encodeByteArray arr =
    BE.sequence
        (BE.unsignedInt32 Bytes.BE (Array.length arr)
            :: (Array.toList arr |> List.map BE.unsignedInt8)
        )


encodeWordList : List Int -> BE.Encoder
encodeWordList words =
    BE.sequence
        (BE.unsignedInt32 Bytes.BE (List.length words)
            :: List.map (\w -> BE.unsignedInt16 Bytes.BE (Bitwise.and w 0xFFFF)) words
        )


encodeFrames : List CallFrame -> BE.Encoder
encodeFrames frames =
    BE.sequence
        (BE.unsignedInt32 Bytes.BE (List.length frames)
            :: List.map encodeFrame frames
        )


encodeFrame : CallFrame -> BE.Encoder
encodeFrame frame =
    let
        ( storeFlag, storeKind, storeNum ) =
            case frame.returnStore of
                Nothing ->
                    ( 0, 0, 0 )

                Just Stack ->
                    ( 1, 0, 0 )

                Just (Local n) ->
                    ( 1, 1, n )

                Just (Global n) ->
                    ( 1, 2, n )

        localsList =
            Array.toList frame.locals
    in
    BE.sequence
        [ BE.unsignedInt32 Bytes.BE frame.returnPC
        , BE.unsignedInt8 storeFlag
        , BE.unsignedInt8 storeKind
        , BE.unsignedInt8 storeNum
        , BE.unsignedInt8 (List.length localsList)
        , BE.sequence (List.map (\w -> BE.unsignedInt16 Bytes.BE (Bitwise.and w 0xFFFF)) localsList)
        , BE.unsignedInt32 Bytes.BE (List.length frame.evalStack)
        , BE.sequence (List.map (\w -> BE.unsignedInt16 Bytes.BE (Bitwise.and w 0xFFFF)) frame.evalStack)
        ]



-- NATIVE DECODE HELPERS


nativeDecoder : BD.Decoder (Result String Snapshot)
nativeDecoder =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen
            (\magic ->
                if magic /= nativeMagic then
                    BD.succeed (Err "Snapshot bytes have wrong magic")

                else
                    BD.unsignedInt8
                        |> BD.andThen
                            (\version ->
                                if version /= nativeVersion then
                                    BD.succeed (Err ("Snapshot format version " ++ String.fromInt version ++ " not supported"))

                                else
                                    decodeBody
                            )
            )


decodeBody : BD.Decoder (Result String Snapshot)
decodeBody =
    BD.unsignedInt8
        |> BD.andThen
            (\resumeByte ->
                case byteToResumeKind resumeByte of
                    Err e ->
                        BD.succeed (Err e)

                    Ok resume ->
                        BD.map5
                            (\release serial checksum pcAddr dynMem ->
                                { resume = resume
                                , release = release
                                , serial = serial
                                , checksum = checksum
                                , pcAddr = pcAddr
                                , dynMem = dynMem
                                }
                            )
                            (BD.unsignedInt16 Bytes.BE)
                            (decodeSerial 6)
                            (BD.unsignedInt16 Bytes.BE)
                            (BD.unsignedInt32 Bytes.BE)
                            decodeByteArray
                            |> BD.andThen
                                (\header ->
                                    BD.map2
                                        (\evalStack frames ->
                                            Ok
                                                (Snapshot
                                                    { dynamicMem = header.dynMem
                                                    , pcAddr = header.pcAddr
                                                    , evalStack = evalStack
                                                    , callFrames = frames
                                                    , resume = header.resume
                                                    , metaData =
                                                        { release = header.release
                                                        , serial = header.serial
                                                        , checksum = header.checksum
                                                        }
                                                    }
                                                )
                                        )
                                        decodeWordList
                                        decodeFrames
                                )
            )


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
    BD.loop ( n, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (String.fromList (List.reverse acc)))

            else
                BD.unsignedInt8
                    |> BD.map (\b -> BD.Loop ( remaining - 1, Char.fromCode b :: acc ))
        )


decodeByteArray : BD.Decoder (Array Int)
decodeByteArray =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen
            (\len ->
                BD.loop ( len, Array.empty )
                    (\( remaining, acc ) ->
                        if remaining <= 0 then
                            BD.succeed (BD.Done acc)

                        else
                            BD.unsignedInt8
                                |> BD.map (\b -> BD.Loop ( remaining - 1, Array.push b acc ))
                    )
            )


decodeWordList : BD.Decoder (List Int)
decodeWordList =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen
            (\len ->
                BD.loop ( len, [] )
                    (\( remaining, acc ) ->
                        if remaining <= 0 then
                            BD.succeed (BD.Done (List.reverse acc))

                        else
                            BD.unsignedInt16 Bytes.BE
                                |> BD.map (\w -> BD.Loop ( remaining - 1, w :: acc ))
                    )
            )


decodeFrames : BD.Decoder (List CallFrame)
decodeFrames =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen
            (\count ->
                BD.loop ( count, [] )
                    (\( remaining, acc ) ->
                        if remaining <= 0 then
                            BD.succeed (BD.Done (List.reverse acc))

                        else
                            decodeFrame
                                |> BD.map (\f -> BD.Loop ( remaining - 1, f :: acc ))
                    )
            )


decodeFrame : BD.Decoder CallFrame
decodeFrame =
    BD.unsignedInt32 Bytes.BE
        |> BD.andThen
            (\returnPC ->
                BD.map3 (\flag kind num -> ( flag, kind, num ))
                    BD.unsignedInt8
                    BD.unsignedInt8
                    BD.unsignedInt8
                    |> BD.andThen
                        (\( storeFlag, storeKind, storeNum ) ->
                            let
                                storeRef =
                                    if storeFlag == 0 then
                                        Nothing

                                    else
                                        case storeKind of
                                            0 ->
                                                Just Stack

                                            1 ->
                                                Just (Local storeNum)

                                            _ ->
                                                Just (Global storeNum)
                            in
                            BD.unsignedInt8
                                |> BD.andThen
                                    (\localsLen ->
                                        decodeWordArray localsLen
                                            |> BD.andThen
                                                (\locals ->
                                                    decodeWordList
                                                        |> BD.map
                                                            (\evalStack ->
                                                                { returnPC = returnPC
                                                                , returnStore = storeRef
                                                                , locals = locals
                                                                , evalStack = evalStack
                                                                }
                                                            )
                                                )
                                    )
                        )
            )


decodeWordArray : Int -> BD.Decoder (Array Int)
decodeWordArray count =
    BD.loop ( count, Array.empty )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done acc)

            else
                BD.unsignedInt16 Bytes.BE
                    |> BD.map (\w -> BD.Loop ( remaining - 1, Array.push w acc ))
        )
