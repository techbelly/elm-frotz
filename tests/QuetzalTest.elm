module QuetzalTest exposing (suite)

import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine
import ZMachine.Execute as Execute
import ZMachine.Memory as Memory
import ZMachine.Quetzal as Quetzal
import ZMachine.Run as Run
import ZMachine.Snapshot as Snapshot
import ZMachine.State as State
import ZMachine.Types
    exposing
        ( StepResult(..)
        , ZMachine
        )


suite : Test
suite =
    describe "Quetzal"
        [ test "encode rejects ResumeAt (autosave) snapshots" <|
            \_ ->
                let
                    zm =
                        makeZM saveBytes

                    snap =
                        ZMachine.snapshot zm
                in
                case Quetzal.encode snap of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "Quetzal should reject autosave snapshots"
        , test "encode then decode round-trips metadata for a save-opcode snapshot" <|
            \_ ->
                case Execute.step (makeZM saveBytes) of
                    NeedSave snap _ ->
                        case Quetzal.encode snap of
                            Err msg ->
                                Expect.fail ("Quetzal.encode failed: " ++ msg)

                            Ok bytes ->
                                case Quetzal.decode bytes of
                                    Err msg ->
                                        Expect.fail ("Quetzal.decode failed: " ++ msg)

                                    Ok decoded ->
                                        Expect.all
                                            [ \d -> Snapshot.pc d |> Expect.equal (Snapshot.pc snap)
                                            , \d -> Snapshot.resumeKind d |> Expect.equal Snapshot.ResumeByBranchTrue
                                            , \d -> (Snapshot.meta d).release |> Expect.equal 1
                                            , \d -> (Snapshot.meta d).serial |> Expect.equal "TEST01"
                                            , \d -> (Snapshot.meta d).checksum |> Expect.equal 0x1234
                                            , \d -> Snapshot.dynamicMemory d |> Expect.equal (Snapshot.dynamicMemory snap)
                                            , \d -> Snapshot.stack d |> Expect.equal (Snapshot.stack snap)
                                            ]
                                            decoded

                    _ ->
                        Expect.fail "expected NeedSave"
        , test "Quetzal-decoded snapshot can be used to resume via provideRestoreResult" <|
            \_ ->
                case Execute.step (makeZM saveBytes) of
                    NeedSave snap _ ->
                        case Quetzal.encode snap |> Result.andThen Quetzal.decode of
                            Err msg ->
                                Expect.fail ("round-trip failed: " ++ msg)

                            Ok decoded ->
                                let
                                    restoreMachine =
                                        makeZM restoreBytes
                                in
                                case Execute.step restoreMachine of
                                    NeedRestore m ->
                                        case Run.provideRestoreResult (Just decoded) m of
                                            Continue next ->
                                                next.pc |> Expect.equal 0x4A

                                            _ ->
                                                Expect.fail "expected Continue"

                                    _ ->
                                        Expect.fail "expected NeedRestore"

                    _ ->
                        Expect.fail "expected NeedSave"
        ]



-- HELPERS


{-| Minimal V3 story with the given instructions placed at PC = 0x40.
Header layout matches other tests:

  - static base 0x100
  - initial PC 0x40
  - globals 0x80
  - release 0x0001
  - serial "TEST01"
  - checksum 0x1234

-}
makeZM : List Int -> ZMachine
makeZM instrBytes =
    let
        pc =
            0x40

        release =
            1

        totalSize =
            512

        staticBase =
            0x0100

        globalsAddr =
            0x80

        checksumValue =
            0x1234

        base =
            List.repeat totalSize 0
                |> setAt 0x00 3
                -- version
                |> setAt 0x02 (Bitwise.shiftRightZfBy 8 release)
                |> setAt 0x03 (Bitwise.and release 0xFF)
                -- release
                |> setAt 0x04 0x01
                |> setAt 0x05 0x00
                -- high memory base
                |> setAt 0x06 (Bitwise.shiftRightZfBy 8 pc)
                |> setAt 0x07 (Bitwise.and pc 0xFF)
                -- initial PC
                |> setAt 0x0C (Bitwise.shiftRightZfBy 8 globalsAddr)
                |> setAt 0x0D (Bitwise.and globalsAddr 0xFF)
                -- globals
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                -- static base
                |> setAt 0x12 (Char.toCode 'T')
                |> setAt 0x13 (Char.toCode 'E')
                |> setAt 0x14 (Char.toCode 'S')
                |> setAt 0x15 (Char.toCode 'T')
                |> setAt 0x16 (Char.toCode '0')
                |> setAt 0x17 (Char.toCode '1')
                -- serial
                |> setAt 0x1C (Bitwise.shiftRightZfBy 8 checksumValue)
                |> setAt 0x1D (Bitwise.and checksumValue 0xFF)

        -- checksum
        withInstr =
            List.foldl
                (\( i, b ) acc -> setAt (pc + i) b acc)
                base
                (List.indexedMap Tuple.pair instrBytes)

        mem =
            withInstr
                |> List.map Encode.unsignedInt8
                |> Encode.sequence
                |> Encode.encode
                |> Memory.fromBytes
                |> unwrap
    in
    State.init mem


setAt : Int -> Int -> List Int -> List Int
setAt index value list =
    List.indexedMap
        (\i v ->
            if i == index then
                value

            else
                v
        )
        list


unwrap : Result String a -> a
unwrap result =
    case result of
        Ok v ->
            v

        Err msg ->
            Debug.todo msg


{-| Bytes for a 0OP `save` instruction whose branch offset jumps 10
forward when taken. Branch byte 0xCA = `bit 7 (branch on true) | bit 6
(single byte) | 0x0A` (offset 10).
-}
saveBytes : List Int
saveBytes =
    [ 0xB5, 0xCA ]


restoreBytes : List Int
restoreBytes =
    [ 0xB6, 0xCA ]
