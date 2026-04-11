module SnapshotTest exposing (suite)

import Array
import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Execute as Execute
import ZMachine.Memory as Memory
import ZMachine.Run as Run
import ZMachine.Snapshot as Snapshot exposing (ResumeKind(..))
import ZMachine.State as State
import ZMachine.Types
    exposing
        ( StepResult(..)
        , ZMachine
        )


suite : Test
suite =
    describe "Save / restore"
        [ captureRestoreTests
        , nativeCodecTests
        , wrongStoryTests
        , saveOpcodeTests
        , restoreOpcodeTests
        ]



-- HELPERS


{-| Minimal V3 story with the given instructions placed at PC = 0x40.
Header layout matches other tests:

  - static base 0x100
  - initial PC 0x40
  - globals 0x80
  - release 0x0001 (so we can build a second story with a different release)
  - serial "TEST01"
  - checksum 0x1234

-}
makeZM : List Int -> ZMachine
makeZM instrBytes =
    makeStoryWithRelease 1 instrBytes


makeStoryWithRelease : Int -> List Int -> ZMachine
makeStoryWithRelease release instrBytes =
    let
        pc =
            0x40

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



-- CAPTURE / RESTORE ROUND-TRIP


captureRestoreTests : Test
captureRestoreTests =
    describe "Snapshot.capture + restore"
        [ test "round-trips basic machine state" <|
            \_ ->
                let
                    baseZM =
                        makeZM saveBytes

                    -- Mutate some state: push a couple of values and move the PC.
                    mutated =
                        { baseZM
                            | pc = 0x45
                            , stack = [ 11, 22, 33 ]
                        }

                    snap =
                        Snapshot.capture
                            { memory = mutated.memory
                            , pc = mutated.pc
                            , stack = mutated.stack
                            , callStack = mutated.callStack
                            , resumeKind = Snapshot.ResumeAt
                            }
                in
                case Snapshot.restore snap baseZM.originalMemory of
                    Err _ ->
                        Expect.fail "restore failed on same-story snapshot"

                    Ok parts ->
                        Expect.all
                            [ \p -> Expect.equal 0x45 p.pc
                            , \p -> Expect.equal [ 11, 22, 33 ] p.stack
                            , \p -> Expect.equal [] p.callStack
                            ]
                            parts
        , test "restore rebuilds dynamic memory from snapshot" <|
            \_ ->
                let
                    baseZM =
                        makeZM saveBytes

                    -- Write to a dynamic-memory address, then capture.
                    mutatedMem =
                        Memory.writeByte 0x80 0xAB baseZM.memory

                    mutated =
                        { baseZM | memory = mutatedMem }

                    snap =
                        Snapshot.capture
                            { memory = mutated.memory
                            , pc = mutated.pc
                            , stack = []
                            , callStack = []
                            , resumeKind = Snapshot.ResumeAt
                            }
                in
                case Snapshot.restore snap baseZM.originalMemory of
                    Err _ ->
                        Expect.fail "restore failed"

                    Ok parts ->
                        Memory.readByte 0x80 parts.memory |> Expect.equal 0xAB
        ]



-- NATIVE CODEC ROUND-TRIP


nativeCodecTests : Test
nativeCodecTests =
    describe "Snapshot native encode/decode"
        [ test "ResumeAt snapshot round-trips through encode → decode → restore" <|
            \_ ->
                let
                    initial =
                        makeZM saveBytes

                    baseZM =
                        { initial | pc = 0x50, stack = [ 7, 8, 9 ] }

                    snap =
                        Snapshot.capture
                            { memory = baseZM.memory
                            , pc = baseZM.pc
                            , stack = baseZM.stack
                            , callStack = baseZM.callStack
                            , resumeKind = Snapshot.ResumeAt
                            }

                    roundTripped =
                        Snapshot.encode snap |> Snapshot.decode
                in
                case roundTripped of
                    Err msg ->
                        Expect.fail ("native decode failed: " ++ msg)

                    Ok decoded ->
                        Expect.all
                            [ \d -> Snapshot.pc d |> Expect.equal 0x50
                            , \d -> Snapshot.stack d |> Expect.equal [ 7, 8, 9 ]
                            , \d -> Snapshot.resumeKind d |> Expect.equal Snapshot.ResumeAt
                            , \d -> (Snapshot.meta d).release |> Expect.equal 1
                            , \d -> (Snapshot.meta d).serial |> Expect.equal "TEST01"
                            , \d -> (Snapshot.meta d).checksum |> Expect.equal 0x1234
                            , \d -> Snapshot.dynamicMemory d |> Array.length |> Expect.equal (Memory.dynamicSize baseZM.memory)
                            ]
                            decoded
        ]



-- WRONG STORY REJECTION


wrongStoryTests : Test
wrongStoryTests =
    describe "Snapshot.restore cross-story safety"
        [ test "rejects snapshot captured from a different release" <|
            \_ ->
                let
                    story1 =
                        makeStoryWithRelease 1 saveBytes

                    story2 =
                        makeStoryWithRelease 2 saveBytes

                    snap =
                        Snapshot.capture
                            { memory = story1.memory
                            , pc = story1.pc
                            , stack = []
                            , callStack = []
                            , resumeKind = Snapshot.ResumeAt
                            }
                in
                case Snapshot.restore snap story2.originalMemory of
                    Err (Snapshot.WrongStory _) ->
                        Expect.pass

                    Err (Snapshot.SnapshotCorrupt _) ->
                        Expect.fail "expected WrongStory, got SnapshotCorrupt"

                    Ok _ ->
                        Expect.fail "expected WrongStory rejection"
        ]



-- SAVE OPCODE INTEGRATION


saveOpcodeTests : Test
saveOpcodeTests =
    describe "save opcode"
        [ test "stepping into save yields NeedSave with the right snapshot" <|
            \_ ->
                case Execute.step (makeZM saveBytes) of
                    NeedSave snap machine ->
                        Expect.all
                            [ \_ -> Snapshot.pc snap |> Expect.equal 0x40
                            , \_ -> Snapshot.resumeKind snap |> Expect.equal Snapshot.ResumeByBranchTrue
                            , \_ -> machine.pc |> Expect.equal 0x40
                            ]
                            ()

                    other ->
                        Expect.fail ("expected NeedSave, got " ++ describeResult other)
        , test "provideSaveResult True takes the branch" <|
            \_ ->
                case Execute.step (makeZM saveBytes) of
                    NeedSave _ machine ->
                        case Run.provideSaveResult True machine of
                            Continue m ->
                                -- save pc 0x40, instr length 2, branch offset 10, pc = 0x42 + 10 - 2 = 0x4A
                                m.pc |> Expect.equal 0x4A

                            _ ->
                                Expect.fail "expected Continue"

                    _ ->
                        Expect.fail "expected NeedSave"
        , test "provideSaveResult False falls through" <|
            \_ ->
                case Execute.step (makeZM saveBytes) of
                    NeedSave _ machine ->
                        case Run.provideSaveResult False machine of
                            Continue m ->
                                m.pc |> Expect.equal 0x42

                            _ ->
                                Expect.fail "expected Continue"

                    _ ->
                        Expect.fail "expected NeedSave"
        ]



-- RESTORE OPCODE INTEGRATION


restoreOpcodeTests : Test
restoreOpcodeTests =
    describe "restore opcode"
        [ test "stepping into restore yields NeedRestore" <|
            \_ ->
                case Execute.step (makeZM restoreBytes) of
                    NeedRestore machine ->
                        machine.pc |> Expect.equal 0x40

                    other ->
                        Expect.fail ("expected NeedRestore, got " ++ describeResult other)
        , test "provideRestoreResult Nothing falls through" <|
            \_ ->
                case Execute.step (makeZM restoreBytes) of
                    NeedRestore machine ->
                        case Run.provideRestoreResult Nothing machine of
                            Continue m ->
                                m.pc |> Expect.equal 0x42

                            _ ->
                                Expect.fail "expected Continue"

                    _ ->
                        Expect.fail "expected NeedRestore"
        , test "provideRestoreResult (Just snapshotFromSave) takes the save branch" <|
            \_ ->
                -- Story A has save at 0x40; story B has restore at 0x40. We
                -- capture from A's save-path snapshot, then feed it to B's
                -- restore-path resumption. Since both stories share the same
                -- header metadata (same release / serial / checksum), restore
                -- succeeds and execution resumes at A's save-branch target.
                let
                    saveMachine =
                        makeZM saveBytes
                in
                case Execute.step saveMachine of
                    NeedSave snap _ ->
                        let
                            restoreMachine =
                                makeZM restoreBytes
                        in
                        case Execute.step restoreMachine of
                            NeedRestore m ->
                                case Run.provideRestoreResult (Just snap) m of
                                    Continue next ->
                                        -- Should have taken the save's branch:
                                        --   nextPC = 0x40 + 2 = 0x42
                                        --   pc after branch = 0x42 + 10 - 2 = 0x4A
                                        next.pc |> Expect.equal 0x4A

                                    _ ->
                                        Expect.fail "expected Continue after restore"

                            _ ->
                                Expect.fail "expected NeedRestore"

                    _ ->
                        Expect.fail "expected NeedSave"
        ]



-- DESCRIBE STEP RESULTS FOR FAILURE MESSAGES


describeResult : StepResult -> String
describeResult result =
    case result of
        Continue _ ->
            "Continue"

        NeedInput _ _ ->
            "NeedInput"

        NeedSave _ _ ->
            "NeedSave"

        NeedRestore _ ->
            "NeedRestore"

        Halted _ ->
            "Halted"

        Error _ _ ->
            "Error"
