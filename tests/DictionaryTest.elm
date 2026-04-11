module DictionaryTest exposing (suite)

import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Dictionary as Dictionary
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Text as Text


suite : Test
suite =
    describe "ZMachine.Dictionary"
        [ tokenizeTests
        , lookupTests
        , parseBufferTests
        ]



-- TEST HELPERS


{-| Build a memory image with a dictionary containing given words.

Memory layout:

  - Header at 0x00 (version=3, static base=0x100)
  - Dictionary at 0x08 (header word)
  - Dictionary placed at 0x40
  - Text buffer at 0xC0
  - Parse buffer at 0xD0
  - Object table at 0x0A (header word) — minimal, not used here

Dictionary format:

  - 1 byte: number of separators
  - n bytes: separator characters
  - 1 byte: entry length (7 = 4 bytes encoded text + 3 bytes data)
  - 2 bytes: number of entries (signed)
  - entries...

-}
makeMemWithDict : List Char -> List String -> Memory
makeMemWithDict separators words =
    let
        totalSize =
            512

        staticBase =
            0x0100

        dictAddr =
            0x40

        textBufAddr =
            0xC0

        parseBufAddr =
            0xD0

        base =
            List.repeat totalSize 0
                |> setAt 0 3
                -- Static base
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                -- Dictionary address
                |> setAt 0x08 (Bitwise.shiftRightZfBy 8 dictAddr)
                |> setAt 0x09 (Bitwise.and dictAddr 0xFF)
                -- Text buffer: max length at byte 0
                |> setAt textBufAddr 80
                -- Parse buffer: max words at byte 0
                |> setAt parseBufAddr 10

        -- Build dictionary
        numSeps =
            List.length separators

        sepBytes =
            List.map Char.toCode separators

        entryLength =
            7

        numEntries =
            List.length words

        -- Encode each word to dictionary form
        entries =
            words
                |> List.map
                    (\w ->
                        let
                            zchars =
                                Text.encodeToZChars w

                            zwords =
                                Text.packZCharsToWords zchars

                            w1 =
                                List.head zwords |> Maybe.withDefault 0

                            w2 =
                                List.head (List.drop 1 zwords) |> Maybe.withDefault 0
                        in
                        [ Bitwise.shiftRightZfBy 8 w1
                        , Bitwise.and w1 0xFF
                        , Bitwise.shiftRightZfBy 8 w2
                        , Bitwise.and w2 0xFF
                        , 0
                        , 0
                        , 0
                        ]
                    )

        dictBytes =
            numSeps
                :: sepBytes
                ++ [ entryLength ]
                ++ [ Bitwise.shiftRightZfBy 8 numEntries, Bitwise.and numEntries 0xFF ]
                ++ List.concat entries

        withDict =
            List.foldl
                (\( i, b ) acc -> setAt (dictAddr + i) b acc)
                base
                (List.indexedMap Tuple.pair dictBytes)
    in
    withDict
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode
        |> Memory.fromBytes
        |> unwrap


unwrap : Result String a -> a
unwrap result =
    case result of
        Ok v ->
            v

        Err msg ->
            Debug.todo msg


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



-- TOKENIZE TESTS


tokenizeTests : Test
tokenizeTests =
    describe "tokenize"
        [ test "simple word gets written to text buffer" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [ ',' ] [ "open" ]

                    result =
                        Dictionary.tokenize "open" 0xC0 0xD0 mem
                in
                -- Text buffer at 0xC1 should contain "open\0"
                List.map (\i -> Memory.readByte (0xC1 + i) result) (List.range 0 4)
                    |> Expect.equal [ Char.toCode 'o', Char.toCode 'p', Char.toCode 'e', Char.toCode 'n', 0 ]
        , test "input is lowercased" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "hello" ]

                    result =
                        Dictionary.tokenize "HELLO" 0xC0 0xD0 mem
                in
                List.map (\i -> Memory.readByte (0xC1 + i) result) (List.range 0 4)
                    |> Expect.equal [ Char.toCode 'h', Char.toCode 'e', Char.toCode 'l', Char.toCode 'l', Char.toCode 'o' ]
        , test "word count written to parse buffer byte 1" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "open", "the", "box" ]

                    result =
                        Dictionary.tokenize "open the box" 0xC0 0xD0 mem
                in
                Memory.readByte (0xD0 + 1) result
                    |> Expect.equal 3
        , test "separator characters split and are kept as words" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [ ',' ] [ "fred", "go" ]

                    result =
                        Dictionary.tokenize "fred,go" 0xC0 0xD0 mem
                in
                -- Should produce 3 tokens: "fred", ",", "go"
                Memory.readByte (0xD0 + 1) result
                    |> Expect.equal 3
        , test "multiple spaces are ignored" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "a", "b" ]

                    result =
                        Dictionary.tokenize "a   b" 0xC0 0xD0 mem
                in
                Memory.readByte (0xD0 + 1) result
                    |> Expect.equal 2
        ]



-- LOOKUP TESTS


lookupTests : Test
lookupTests =
    describe "lookupWord"
        [ test "finds a word in sorted dictionary" <|
            \_ ->
                let
                    -- Words must be in sorted order (by encoded form)
                    mem =
                        makeMemWithDict [ ',' ] [ "box", "open", "the" ]

                    result =
                        Dictionary.lookupWord "open" mem
                in
                -- Should find it (nonzero address)
                (result /= 0) |> Expect.equal True
        , test "returns 0 for unknown word" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [ ',' ] [ "box", "open", "the" ]

                    result =
                        Dictionary.lookupWord "xyzzy" mem
                in
                result |> Expect.equal 0
        , test "lookup is case-insensitive (encodeToZChars lowercases)" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "hello" ]

                    r1 =
                        Dictionary.lookupWord "hello" mem

                    r2 =
                        Dictionary.lookupWord "HELLO" mem
                in
                (r1 == r2 && r1 /= 0) |> Expect.equal True
        , test "words longer than 6 zchars match on first 6" <|
            \_ ->
                let
                    -- "abcdef" encodes to 6 zchars; "abcdefgh" also encodes to same 6
                    mem =
                        makeMemWithDict [] [ "abcdef" ]

                    result =
                        Dictionary.lookupWord "abcdefgh" mem
                in
                (result /= 0) |> Expect.equal True
        ]



-- PARSE BUFFER TESTS


parseBufferTests : Test
parseBufferTests =
    describe "parse buffer format"
        [ test "first token entry has correct dict addr, length, position" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "open" ]

                    result =
                        Dictionary.tokenize "open" 0xC0 0xD0 mem

                    -- Parse buffer: byte 0 = max, byte 1 = count
                    -- Token 0 at offset 2: dict addr (2 bytes), length (1), position (1)
                    tokenDictAddr =
                        Memory.readWord (0xD0 + 2) result

                    tokenLength =
                        Memory.readByte (0xD0 + 4) result

                    tokenPosition =
                        Memory.readByte (0xD0 + 5) result
                in
                ( tokenDictAddr /= 0, tokenLength, tokenPosition )
                    |> Expect.equal ( True, 4, 1 )
        , test "second token starts at correct position" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "go", "north" ]

                    result =
                        Dictionary.tokenize "go north" 0xC0 0xD0 mem

                    -- Token 1 at offset 6
                    tokenPosition =
                        Memory.readByte (0xD0 + 9) result
                in
                -- "go north": "north" starts at position 3 (0-indexed), +1 for 1-indexed = 4
                tokenPosition |> Expect.equal 4
        , test "unknown word gets dict addr 0" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "open" ]

                    result =
                        Dictionary.tokenize "xyzzy" 0xC0 0xD0 mem

                    tokenDictAddr =
                        Memory.readWord (0xD0 + 2) result
                in
                tokenDictAddr |> Expect.equal 0
        , test "max words limit is respected" <|
            \_ ->
                let
                    mem =
                        makeMemWithDict [] [ "a", "b", "c", "d", "e" ]
                            |> Memory.writeByte 0xD0 3

                    -- Max 3 words
                    result =
                        Dictionary.tokenize "a b c d e" 0xC0 0xD0 mem
                in
                Memory.readByte (0xD0 + 1) result
                    |> Expect.equal 3
        ]
