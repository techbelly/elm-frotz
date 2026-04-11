module ZMachine.Quetzal exposing
    ( encode
    , decode
    )

{-| Quetzal save-file format for the Z-Machine.

[Quetzal](https://inform-fiction.org/zmachine/standards/quetzal/) is the
standard IFF container for Z-Machine save files. Implementing it lets
saves round-trip with other interpreters (Frotz, Bocfel, Gargoyle…).

This module is a thin codec between an opaque
[`ZMachine.Snapshot.Snapshot`](ZMachine-Snapshot#Snapshot) and raw
Quetzal bytes. For autosave-style snapshots (tagged
[`ResumeAt`](ZMachine-Snapshot#ResumeKind)), [`encode`](#encode) returns
an error — Quetzal has no way to represent "resume at an arbitrary
instruction boundary"; use
[`ZMachine.Snapshot.encode`](ZMachine-Snapshot#encode) instead.

The current implementation writes UMem (uncompressed dynamic memory)
chunks only — CMem (delta-and-RLE) can be added later without breaking
the API.

@docs encode, decode

-}

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as BD
import Bytes.Encode as BE
import ZMachine.Opcode exposing (VariableRef(..))
import ZMachine.Snapshot as Snapshot exposing (ResumeKind(..), Snapshot, SnapshotMeta)
import ZMachine.Stack exposing (CallFrame)


{-| Encode a snapshot as Quetzal bytes.

Returns `Err` if the snapshot was produced by the host (an autosave) —
Quetzal cannot represent resuming at an arbitrary instruction boundary.
Use [`ZMachine.Snapshot.encode`](ZMachine-Snapshot#encode) for those.

-}
encode : Snapshot -> Result String Bytes
encode snap =
    case Snapshot.resumeKind snap of
        ResumeAt ->
            Err
                "Quetzal cannot encode an autosave (host-initiated snapshot); use ZMachine.Snapshot.encode instead"

        ResumeByBranchTrue ->
            Ok (encodeForm snap)


{-| Decode Quetzal bytes into a snapshot. Always produces a
`ResumeByBranchTrue` snapshot — Quetzal only represents opcode-initiated
saves.

Returns `Err` with a reason string if the file is malformed or uses
features this implementation does not yet support (currently: CMem).

The returned snapshot carries the metadata from the Quetzal file's
`IFhd` chunk, so you can compare it against the current story's
metadata before passing it to
[`ZMachine.Snapshot.restore`](ZMachine-Snapshot#restore).

-}
decode : Bytes -> Result String Snapshot
decode bytes =
    case BD.decode decodeFile bytes of
        Just result ->
            result

        Nothing ->
            Err "Quetzal bytes truncated or malformed"



-- ENCODING


encodeForm : Snapshot -> Bytes
encodeForm snap =
    let
        ifhd =
            encodeChunk "IFhd" (ifhdEncoder snap)

        umem =
            encodeChunk "UMem" (umemEncoder snap)

        stks =
            encodeChunk "Stks" (stksEncoder snap)

        body =
            BE.encode
                (BE.sequence
                    [ encodeTag "IFZS"
                    , BE.bytes ifhd
                    , BE.bytes umem
                    , BE.bytes stks
                    ]
                )
    in
    BE.encode
        (BE.sequence
            [ encodeTag "FORM"
            , BE.unsignedInt32 Bytes.BE (Bytes.width body)
            , BE.bytes body
            ]
        )


encodeTag : String -> BE.Encoder
encodeTag tag =
    let
        codes =
            tag
                |> String.toList
                |> List.map Char.toCode
                |> padTo 4 0x20
    in
    BE.sequence (List.map BE.unsignedInt8 codes)


padTo : Int -> Int -> List Int -> List Int
padTo n fill xs =
    let
        current =
            List.length xs
    in
    if current >= n then
        List.take n xs

    else
        xs ++ List.repeat (n - current) fill


encodeChunk : String -> BE.Encoder -> Bytes
encodeChunk tag dataEncoder =
    let
        dataBytes =
            BE.encode dataEncoder

        len =
            Bytes.width dataBytes

        padding =
            if modBy 2 len /= 0 then
                BE.unsignedInt8 0

            else
                BE.sequence []
    in
    BE.encode
        (BE.sequence
            [ encodeTag tag
            , BE.unsignedInt32 Bytes.BE len
            , BE.bytes dataBytes
            , padding
            ]
        )


ifhdEncoder : Snapshot -> BE.Encoder
ifhdEncoder snap =
    let
        m =
            Snapshot.meta snap
    in
    BE.sequence
        [ BE.unsignedInt16 Bytes.BE m.release
        , encodeSerial m.serial
        , BE.unsignedInt16 Bytes.BE m.checksum

        -- Quetzal stores PC at the branch byte of the save instruction.
        -- For V3 0OP Save the opcode byte is 1 byte, so branchAddr = opcodeAddr + 1.
        , encode3ByteAddr (Snapshot.pc snap + 1)
        ]


encodeSerial : String -> BE.Encoder
encodeSerial s =
    let
        padded =
            String.left 6 (s ++ "      ")
    in
    padded
        |> String.toList
        |> List.map (Char.toCode >> BE.unsignedInt8)
        |> BE.sequence


encode3ByteAddr : Int -> BE.Encoder
encode3ByteAddr addr =
    BE.sequence
        [ BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 16 addr) 0xFF)
        , BE.unsignedInt8 (Bitwise.and (Bitwise.shiftRightZfBy 8 addr) 0xFF)
        , BE.unsignedInt8 (Bitwise.and addr 0xFF)
        ]


umemEncoder : Snapshot -> BE.Encoder
umemEncoder snap =
    Snapshot.dynamicMemory snap
        |> Array.toList
        |> List.map BE.unsignedInt8
        |> BE.sequence


stksEncoder : Snapshot -> BE.Encoder
stksEncoder snap =
    let
        ( dummyEvalStack, realFrames ) =
            buildQuetzalFrames (Snapshot.callStack snap) (Snapshot.stack snap)

        dummy =
            { returnPC = 0
            , discardResult = True
            , resultVariable = 0
            , argsSupplied = 0
            , locals = []
            , evalStack = dummyEvalStack
            }
    in
    BE.sequence (List.map qframeEncoder (dummy :: realFrames))


type alias QuetzalFrame =
    { returnPC : Int
    , discardResult : Bool
    , resultVariable : Int
    , argsSupplied : Int
    , locals : List Int
    , evalStack : List Int
    }


{-| Walk our callStack (newest-first) and produce the Quetzal frame
list (oldest-first), along with the eval stack to use for the dummy
"main" frame at the bottom.

In our representation, `frame.evalStack` is the eval stack of the
routine that was interrupted when the call happened — which in Quetzal
terms belongs to the frame directly below it on the call stack. So the
newest frame's eval stack is `machine.stack`, and each older frame
takes its eval stack from the frame directly above it.

-}
buildQuetzalFrames : List CallFrame -> List Int -> ( List Int, List QuetzalFrame )
buildQuetzalFrames frames mainStack =
    let
        go remaining current acc =
            case remaining of
                [] ->
                    ( current, acc )

                f :: rest ->
                    go rest f.evalStack (makeQFrame f current :: acc)
    in
    go frames mainStack []


makeQFrame : CallFrame -> List Int -> QuetzalFrame
makeQFrame frame evalStack =
    let
        ( discardResult, resultVariable ) =
            case frame.returnStore of
                Nothing ->
                    ( True, 0 )

                Just Stack ->
                    ( False, 0 )

                Just (Local n) ->
                    ( False, n )

                Just (Global n) ->
                    ( False, n )
    in
    { returnPC = frame.returnPC
    , discardResult = discardResult
    , resultVariable = resultVariable

    -- Z-Machine V3 has no check_arg_count opcode, so the "args supplied"
    -- bitmap is never inspected after restore. Zero is fine.
    , argsSupplied = 0
    , locals = Array.toList frame.locals
    , evalStack = evalStack
    }


qframeEncoder : QuetzalFrame -> BE.Encoder
qframeEncoder f =
    let
        numLocals =
            List.length f.locals

        flags =
            Bitwise.or
                (Bitwise.and numLocals 0x0F)
                (if f.discardResult then
                    0x10

                 else
                    0
                )
    in
    BE.sequence
        [ encode3ByteAddr f.returnPC
        , BE.unsignedInt8 flags
        , BE.unsignedInt8 f.resultVariable
        , BE.unsignedInt8 f.argsSupplied
        , BE.unsignedInt16 Bytes.BE (List.length f.evalStack)
        , BE.sequence (List.map encodeWord f.locals)
        , BE.sequence (List.map encodeWord f.evalStack)
        ]


encodeWord : Int -> BE.Encoder
encodeWord w =
    BE.unsignedInt16 Bytes.BE (Bitwise.and w 0xFFFF)



-- DECODING


decodeFile : BD.Decoder (Result String Snapshot)
decodeFile =
    decodeTag
        |> BD.andThen
            (\formTag ->
                if formTag /= "FORM" then
                    BD.succeed (Err ("Expected 'FORM', got '" ++ formTag ++ "'"))

                else
                    BD.unsignedInt32 Bytes.BE
                        |> BD.andThen
                            (\formLen ->
                                decodeTag
                                    |> BD.andThen
                                        (\typeTag ->
                                            if typeTag /= "IFZS" then
                                                BD.succeed (Err ("Expected 'IFZS', got '" ++ typeTag ++ "'"))

                                            else
                                                -- formLen covers the IFZS tag (4 bytes) plus all chunks.
                                                decodeChunks (formLen - 4) emptyState
                                                    |> BD.map finalizeState
                                        )
                            )
            )


decodeTag : BD.Decoder String
decodeTag =
    BD.map4 (\a b c d -> String.fromList [ Char.fromCode a, Char.fromCode b, Char.fromCode c, Char.fromCode d ])
        BD.unsignedInt8
        BD.unsignedInt8
        BD.unsignedInt8
        BD.unsignedInt8



-- Accumulator for chunks we've parsed so far


type alias DecodeState =
    { ifhd : Maybe { meta : SnapshotMeta, branchByteAddr : Int }
    , umem : Maybe (Array Int)
    , stks : Maybe (List QuetzalFrame)
    , error : Maybe String
    }


emptyState : DecodeState
emptyState =
    { ifhd = Nothing, umem = Nothing, stks = Nothing, error = Nothing }


decodeChunks : Int -> DecodeState -> BD.Decoder DecodeState
decodeChunks remaining state =
    case state.error of
        Just _ ->
            BD.succeed state

        Nothing ->
            if remaining <= 0 then
                BD.succeed state

            else
                BD.map2 Tuple.pair decodeTag (BD.unsignedInt32 Bytes.BE)
                    |> BD.andThen
                        (\( tag, len ) ->
                            let
                                paddedLen =
                                    len + modBy 2 len
                            in
                            BD.bytes len
                                |> BD.andThen
                                    (\chunkBytes ->
                                        -- consume padding byte if any
                                        BD.bytes (paddedLen - len)
                                            |> BD.map (\_ -> chunkBytes)
                                    )
                                |> BD.andThen
                                    (\chunkBytes ->
                                        decodeChunks
                                            (remaining - 8 - paddedLen)
                                            (applyChunk tag chunkBytes state)
                                    )
                        )


applyChunk : String -> Bytes -> DecodeState -> DecodeState
applyChunk tag chunkBytes state =
    case tag of
        "IFhd" ->
            case BD.decode ifhdDecoder chunkBytes of
                Just result ->
                    { state | ifhd = Just result }

                Nothing ->
                    { state | error = Just "IFhd chunk malformed" }

        "UMem" ->
            { state | umem = Just (bytesToArray chunkBytes) }

        "CMem" ->
            { state | error = Just "CMem (compressed memory) is not yet supported; use UMem" }

        "Stks" ->
            case BD.decode (stksDecoder (Bytes.width chunkBytes)) chunkBytes of
                Just frames ->
                    { state | stks = Just frames }

                Nothing ->
                    { state | error = Just "Stks chunk malformed" }

        _ ->
            -- Unknown chunks are ignored, per IFF convention
            state


finalizeState : DecodeState -> Result String Snapshot
finalizeState state =
    case state.error of
        Just err ->
            Err err

        Nothing ->
            case ( state.ifhd, state.umem, state.stks ) of
                ( Just header, Just dyn, Just frames ) ->
                    let
                        ( stackList, callStack ) =
                            quetzalFramesToInternal frames
                    in
                    Ok
                        (Snapshot.fromParts
                            { dynamicMemory = dyn
                            , pc = header.branchByteAddr - 1
                            , stack = stackList
                            , callStack = callStack
                            , resumeKind = ResumeByBranchTrue
                            , meta = header.meta
                            }
                        )

                ( Nothing, _, _ ) ->
                    Err "Quetzal file missing IFhd chunk"

                ( _, Nothing, _ ) ->
                    Err "Quetzal file missing UMem chunk (CMem not yet supported)"

                ( _, _, Nothing ) ->
                    Err "Quetzal file missing Stks chunk"


ifhdDecoder : BD.Decoder { meta : SnapshotMeta, branchByteAddr : Int }
ifhdDecoder =
    BD.map4
        (\release serial checksum addr ->
            { meta =
                { release = release
                , serial = serial
                , checksum = checksum
                }
            , branchByteAddr = addr
            }
        )
        (BD.unsignedInt16 Bytes.BE)
        (decodeFixedString 6)
        (BD.unsignedInt16 Bytes.BE)
        decode3ByteAddr


decodeFixedString : Int -> BD.Decoder String
decodeFixedString n =
    BD.loop ( n, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (String.fromList (List.reverse acc)))

            else
                BD.unsignedInt8
                    |> BD.map (\b -> BD.Loop ( remaining - 1, Char.fromCode b :: acc ))
        )


decode3ByteAddr : BD.Decoder Int
decode3ByteAddr =
    BD.map3
        (\a b c ->
            Bitwise.or (Bitwise.shiftLeftBy 16 a)
                (Bitwise.or (Bitwise.shiftLeftBy 8 b) c)
        )
        BD.unsignedInt8
        BD.unsignedInt8
        BD.unsignedInt8


bytesToArray : Bytes -> Array Int
bytesToArray b =
    let
        len =
            Bytes.width b

        decoder =
            BD.loop ( len, Array.empty )
                (\( remaining, acc ) ->
                    if remaining <= 0 then
                        BD.succeed (BD.Done acc)

                    else
                        BD.unsignedInt8
                            |> BD.map (\byte -> BD.Loop ( remaining - 1, Array.push byte acc ))
                )
    in
    BD.decode decoder b |> Maybe.withDefault Array.empty


stksDecoder : Int -> BD.Decoder (List QuetzalFrame)
stksDecoder totalBytes =
    -- Decode frames until we've consumed exactly `totalBytes` of data.
    -- Each frame reports its own on-disk size so we can tick the counter
    -- down without peeking at the bytes stream.
    BD.loop ( totalBytes, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (List.reverse acc))

            else
                qframeDecoderWithSize
                    |> BD.map
                        (\( size, f ) ->
                            BD.Loop ( remaining - size, f :: acc )
                        )
        )


qframeDecoderWithSize : BD.Decoder ( Int, QuetzalFrame )
qframeDecoderWithSize =
    decode3ByteAddr
        |> BD.andThen
            (\returnPC ->
                BD.map3 (\flags resultVar argsSupplied -> ( flags, resultVar, argsSupplied ))
                    BD.unsignedInt8
                    BD.unsignedInt8
                    BD.unsignedInt8
                    |> BD.andThen
                        (\( flags, resultVar, argsSupplied ) ->
                            let
                                numLocals =
                                    Bitwise.and flags 0x0F

                                discardResult =
                                    Bitwise.and flags 0x10 /= 0
                            in
                            BD.unsignedInt16 Bytes.BE
                                |> BD.andThen
                                    (\evalStackLen ->
                                        decodeWordListFixed numLocals
                                            |> BD.andThen
                                                (\locals ->
                                                    decodeWordListFixed evalStackLen
                                                        |> BD.map
                                                            (\evalStack ->
                                                                let
                                                                    frame =
                                                                        { returnPC = returnPC
                                                                        , discardResult = discardResult
                                                                        , resultVariable = resultVar
                                                                        , argsSupplied = argsSupplied
                                                                        , locals = locals
                                                                        , evalStack = evalStack
                                                                        }

                                                                    size =
                                                                        8 + 2 * numLocals + 2 * evalStackLen
                                                                in
                                                                ( size, frame )
                                                            )
                                                )
                                    )
                        )
            )


decodeWordListFixed : Int -> BD.Decoder (List Int)
decodeWordListFixed count =
    BD.loop ( count, [] )
        (\( remaining, acc ) ->
            if remaining <= 0 then
                BD.succeed (BD.Done (List.reverse acc))

            else
                BD.unsignedInt16 Bytes.BE
                    |> BD.map (\w -> BD.Loop ( remaining - 1, w :: acc ))
        )


{-| Convert Quetzal's frame list (oldest-first, with a dummy main frame
at index 0) back to our internal representation: the top-level eval
stack (from the currently-executing routine) plus a call-stack list
(newest-first).

Inverse of [`buildQuetzalFrames`](#buildQuetzalFrames).

-}
quetzalFramesToInternal : List QuetzalFrame -> ( List Int, List CallFrame )
quetzalFramesToInternal frames =
    case frames of
        [] ->
            ( [], [] )

        dummy :: rest ->
            case List.reverse rest of
                [] ->
                    -- No real frames: main is executing, eval stack is the dummy's.
                    ( dummy.evalStack, [] )

                topFrame :: _ ->
                    let
                        -- Walk oldest→newest (= `rest` as-is). Each internal frame's
                        -- evalStack is the eval stack of the Quetzal frame directly
                        -- below it in the file (the dummy for the first real frame).
                        -- Prepending to acc yields newest-first, as our callStack
                        -- representation requires.
                        go : List QuetzalFrame -> List Int -> List CallFrame -> List CallFrame
                        go remaining prevEvalStack acc =
                            case remaining of
                                [] ->
                                    acc

                                f :: more ->
                                    go more f.evalStack (toInternalFrame f prevEvalStack :: acc)
                    in
                    ( topFrame.evalStack, go rest dummy.evalStack [] )


toInternalFrame : QuetzalFrame -> List Int -> CallFrame
toInternalFrame f prevEvalStack =
    { returnPC = f.returnPC
    , returnStore = toReturnStore f
    , locals = Array.fromList f.locals
    , evalStack = prevEvalStack
    }


toReturnStore : QuetzalFrame -> Maybe VariableRef
toReturnStore f =
    if f.discardResult then
        Nothing

    else if f.resultVariable == 0 then
        Just Stack

    else if f.resultVariable <= 0x0F then
        Just (Local f.resultVariable)

    else
        Just (Global f.resultVariable)
