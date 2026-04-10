module HeaderTest exposing (suite)

import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Memory.Header as Header


suite : Test
suite =
    describe "ZMachine.Memory.Header"
        [ fieldReadTests
        , flags1Tests
        , flags2Tests
        , interpreterFieldTests
        , serialNumberTests
        , fileLengthTests
        ]



-- TEST HELPERS


{-| Build a V3 story file (256 bytes, static base 128) with patches at specific addresses.
-}
makeStory : List ( Int, Int ) -> Memory
makeStory patches =
    let
        baseBytes =
            List.repeat 256 0

        withHeader =
            baseBytes
                |> setAt 0 3
                |> setAt 14 0
                |> setAt 15 128

        patched =
            List.foldl (\( addr, val ) bs -> setAt addr val bs) withHeader patches
    in
    patched
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode
        |> Memory.fromBytes
        |> unwrap


{-| Build a story with a complete realistic header.
-}
realisticStory : Memory
realisticStory =
    makeStory
        [ -- Release number = 88 (0x0058)
          ( 0x02, 0x00 )
        , ( 0x03, 0x58 )

        -- High memory base = 0x4000
        , ( 0x04, 0x40 )
        , ( 0x05, 0x00 )

        -- Initial PC = 0x5000
        , ( 0x06, 0x50 )
        , ( 0x07, 0x00 )

        -- Dictionary = 0x3000
        , ( 0x08, 0x30 )
        , ( 0x09, 0x00 )

        -- Object table = 0x0100
        , ( 0x0A, 0x01 )
        , ( 0x0B, 0x00 )

        -- Global variables = 0x2000
        , ( 0x0C, 0x20 )
        , ( 0x0D, 0x00 )

        -- Static memory base = 0x0080 (already set)
        -- Flags 2 = 0x00
        -- Abbreviations table = 0x0040
        , ( 0x18, 0x00 )
        , ( 0x19, 0x40 )

        -- Serial number = "860101"
        , ( 0x12, 0x38 )
        , ( 0x13, 0x36 )
        , ( 0x14, 0x30 )
        , ( 0x15, 0x31 )
        , ( 0x16, 0x30 )
        , ( 0x17, 0x31 )

        -- File length = 0x0040 (= 64, actual = 64 * 2 = 128)
        , ( 0x1A, 0x00 )
        , ( 0x1B, 0x40 )

        -- Checksum = 0xABCD
        , ( 0x1C, 0xAB )
        , ( 0x1D, 0xCD )
        ]


unwrap : Result String Memory -> Memory
unwrap result =
    case result of
        Ok mem ->
            mem

        Err msg ->
            Debug.todo ("Failed to load story: " ++ msg)


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



-- TESTS


fieldReadTests : Test
fieldReadTests =
    describe "field reads"
        [ test "version" <|
            \_ ->
                Header.version realisticStory
                    |> Expect.equal 3
        , test "highMemoryBase" <|
            \_ ->
                Header.highMemoryBase realisticStory
                    |> Expect.equal 0x4000
        , test "initialPC" <|
            \_ ->
                Header.initialPC realisticStory
                    |> Expect.equal 0x5000
        , test "dictionaryAddress" <|
            \_ ->
                Header.dictionaryAddress realisticStory
                    |> Expect.equal 0x3000
        , test "objectTableAddress" <|
            \_ ->
                Header.objectTableAddress realisticStory
                    |> Expect.equal 0x0100
        , test "globalVariablesAddress" <|
            \_ ->
                Header.globalVariablesAddress realisticStory
                    |> Expect.equal 0x2000
        , test "staticMemoryBase" <|
            \_ ->
                Header.staticMemoryBase realisticStory
                    |> Expect.equal 0x80
        , test "abbreviationsTableAddress" <|
            \_ ->
                Header.abbreviationsTableAddress realisticStory
                    |> Expect.equal 0x40
        , test "releaseNumber" <|
            \_ ->
                Header.releaseNumber realisticStory
                    |> Expect.equal 0x58
        , test "checksum" <|
            \_ ->
                Header.checksum realisticStory
                    |> Expect.equal 0xABCD
        ]


serialNumberTests : Test
serialNumberTests =
    describe "serialNumber"
        [ test "reads 6 ASCII characters" <|
            \_ ->
                Header.serialNumber realisticStory
                    |> Expect.equal "860101"
        , test "all zeros gives null characters" <|
            \_ ->
                let
                    mem =
                        makeStory []
                in
                Header.serialNumber mem
                    |> Expect.equal "\u{0000}\u{0000}\u{0000}\u{0000}\u{0000}\u{0000}"
        ]


fileLengthTests : Test
fileLengthTests =
    describe "fileLength"
        [ test "V3 multiplies stored value by 2" <|
            \_ ->
                Header.fileLength realisticStory
                    |> Expect.equal 128
        , test "zero stored means zero length" <|
            \_ ->
                let
                    mem =
                        makeStory []
                in
                Header.fileLength mem
                    |> Expect.equal 0
        ]


flags1Tests : Test
flags1Tests =
    describe "Flags 1"
        [ test "no flags set by default" <|
            \_ ->
                let
                    mem =
                        makeStory []
                in
                Header.testFlag1 Header.StatusLineType mem
                    |> Expect.equal False
        , test "StatusLineType set when bit 1 is on" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x01, 0x02 ) ]
                in
                Header.testFlag1 Header.StatusLineType mem
                    |> Expect.equal True
        , test "StoryFileSplit set when bit 2 is on" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x01, 0x04 ) ]
                in
                Header.testFlag1 Header.StoryFileSplit mem
                    |> Expect.equal True
        , test "StatusLineNotAvailable set when bit 4 is on" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x01, 0x10 ) ]
                in
                Header.testFlag1 Header.StatusLineNotAvailable mem
                    |> Expect.equal True
        , test "ScreenSplitAvailable set when bit 5 is on" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x01, 0x20 ) ]
                in
                Header.testFlag1 Header.ScreenSplitAvailable mem
                    |> Expect.equal True
        , test "VariablePitchDefault set when bit 6 is on" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x01, 0x40 ) ]
                in
                Header.testFlag1 Header.VariablePitchDefault mem
                    |> Expect.equal True
        , test "multiple flags can coexist" <|
            \_ ->
                let
                    -- bits 1 and 5 set = 0x22
                    mem =
                        makeStory [ ( 0x01, 0x22 ) ]
                in
                ( Header.testFlag1 Header.StatusLineType mem
                , Header.testFlag1 Header.ScreenSplitAvailable mem
                , Header.testFlag1 Header.StoryFileSplit mem
                )
                    |> Expect.equal ( True, True, False )
        ]


flags2Tests : Test
flags2Tests =
    describe "Flags 2"
        [ test "Transcripting not set by default" <|
            \_ ->
                let
                    mem =
                        makeStory []
                in
                Header.testFlag2 Header.Transcripting mem
                    |> Expect.equal False
        , test "setFlag2 Transcripting" <|
            \_ ->
                let
                    mem =
                        makeStory []
                            |> Header.setFlag2 Header.Transcripting
                in
                Header.testFlag2 Header.Transcripting mem
                    |> Expect.equal True
        , test "clearFlag2 Transcripting" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x10, 0x01 ) ]
                            |> Header.clearFlag2 Header.Transcripting
                in
                Header.testFlag2 Header.Transcripting mem
                    |> Expect.equal False
        , test "setFlag2 ForceFixedPitch" <|
            \_ ->
                let
                    mem =
                        makeStory []
                            |> Header.setFlag2 Header.ForceFixedPitch
                in
                Header.testFlag2 Header.ForceFixedPitch mem
                    |> Expect.equal True
        , test "setting one flag doesn't affect another" <|
            \_ ->
                let
                    mem =
                        makeStory []
                            |> Header.setFlag2 Header.Transcripting
                in
                Header.testFlag2 Header.ForceFixedPitch mem
                    |> Expect.equal False
        , test "clearing one flag doesn't affect another" <|
            \_ ->
                let
                    mem =
                        makeStory [ ( 0x10, 0x03 ) ]
                            |> Header.clearFlag2 Header.Transcripting
                in
                Header.testFlag2 Header.ForceFixedPitch mem
                    |> Expect.equal True
        ]


interpreterFieldTests : Test
interpreterFieldTests =
    describe "interpreter-set fields"
        [ test "setInterpreterInfo writes number and version" <|
            \_ ->
                let
                    mem =
                        makeStory []
                            |> Header.setInterpreterInfo 6 (Char.toCode 'A')
                in
                ( Memory.readByte 0x1E mem, Memory.readByte 0x1F mem )
                    |> Expect.equal ( 6, 65 )
        , test "setScreenSize writes height and width" <|
            \_ ->
                let
                    mem =
                        makeStory []
                            |> Header.setScreenSize 24 80
                in
                ( Memory.readByte 0x20 mem, Memory.readByte 0x21 mem )
                    |> Expect.equal ( 24, 80 )
        , test "setStandardRevision writes major.minor" <|
            \_ ->
                let
                    mem =
                        makeStory []
                            |> Header.setStandardRevision 1 1
                in
                Header.standardRevision mem
                    |> Expect.equal ( 1, 1 )
        ]
