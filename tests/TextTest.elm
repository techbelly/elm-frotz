module TextTest exposing (suite)

import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Text as Text


suite : Test
suite =
    describe "ZMachine.Text"
        [ decodeBasicTests
        , decodeAlphabetTests
        , decodeShiftTests
        , decodeSpecialTests
        , abbreviationTests
        , zsciiTests
        , encodeTests
        , decodeZStringWordsTests
        ]



-- TEST HELPERS


{-| Build a memory image with Z-string words at a given address and an
abbreviations table. Static base at 256, total 512 bytes.
Abbreviation table is placed at address 64 (header word 0x18).
-}
makeMemWithZString : Int -> List Int -> Memory
makeMemWithZString addr zwords =
    makeMemWithZStringAndAbbrs addr zwords []


{-| Same but also place abbreviation strings in memory.
Each entry in `abbrs` is (index, stringAddr, zwords) where index is 0-95.
-}
makeMemWithZStringAndAbbrs : Int -> List Int -> List ( Int, Int, List Int ) -> Memory
makeMemWithZStringAndAbbrs addr zwords abbrs =
    let
        totalSize =
            512

        staticBase =
            256

        abbrTableAddr =
            64

        base =
            List.repeat totalSize 0
                |> setAt 0 3
                -- Static base
                |> setAt 14 (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 15 (Bitwise.and staticBase 0xFF)
                -- Abbreviation table address (word at 0x18)
                |> setAt 0x18 (Bitwise.shiftRightZfBy 8 abbrTableAddr)
                |> setAt 0x19 (Bitwise.and abbrTableAddr 0xFF)

        -- Place Z-string words at addr
        withZString =
            List.foldl
                (\( i, word ) acc ->
                    acc
                        |> setAt (addr + i * 2) (Bitwise.shiftRightZfBy 8 word)
                        |> setAt (addr + i * 2 + 1) (Bitwise.and word 0xFF)
                )
                base
                (List.indexedMap Tuple.pair zwords)

        -- Place abbreviation table entries and their strings
        withAbbrs =
            List.foldl
                (\( abbrIndex, strAddr, strWords ) acc ->
                    let
                        -- Abbreviation table stores word addresses (byte addr / 2)
                        wordAddr =
                            strAddr // 2

                        tableEntryAddr =
                            abbrTableAddr + abbrIndex * 2

                        withEntry =
                            acc
                                |> setAt tableEntryAddr (Bitwise.shiftRightZfBy 8 wordAddr)
                                |> setAt (tableEntryAddr + 1) (Bitwise.and wordAddr 0xFF)
                    in
                    List.foldl
                        (\( j, w ) a ->
                            a
                                |> setAt (strAddr + j * 2) (Bitwise.shiftRightZfBy 8 w)
                                |> setAt (strAddr + j * 2 + 1) (Bitwise.and w 0xFF)
                        )
                        withEntry
                        (List.indexedMap Tuple.pair strWords)
                )
                withZString
                abbrs
    in
    withAbbrs
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode
        |> Memory.fromBytes
        |> unwrap


{-| Pack 3 Z-characters into a word. Set end bit if isLast.
-}
zword : Int -> Int -> Int -> Bool -> Int
zword z1 z2 z3 isLast =
    let
        top =
            if isLast then
                0x8000

            else
                0
    in
    top
        |> Bitwise.or (Bitwise.shiftLeftBy 10 z1)
        |> Bitwise.or (Bitwise.shiftLeftBy 5 z2)
        |> Bitwise.or z3


{-| Minimal V5 memory for encoding tests.
-}
makeV5Mem : Memory
makeV5Mem =
    List.repeat 512 0
        |> setAt 0 5
        |> setAt 14 0x01
        |> setAt 15 0x00
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



-- BASIC DECODING


decodeBasicTests : Test
decodeBasicTests =
    describe "basic decoding"
        [ test "single space (zchar 0)" <|
            \_ ->
                let
                    -- 3 spaces: zchar 0, 0, 0 with end bit
                    mem =
                        makeMemWithZString 200 [ zword 0 0 0 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "   "
        , test "simple lowercase letters" <|
            \_ ->
                let
                    -- "abc" = A0 chars 6, 7, 8
                    mem =
                        makeMemWithZString 200 [ zword 6 7 8 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "abc"
        , test "bytes consumed for single word" <|
            \_ ->
                let
                    mem =
                        makeMemWithZString 200 [ zword 6 7 8 True ]

                    ( _, bytes ) =
                        Text.decodeZString 200 mem
                in
                bytes |> Expect.equal 2
        , test "bytes consumed for two words" <|
            \_ ->
                let
                    mem =
                        makeMemWithZString 200 [ zword 6 7 8 False, zword 9 10 11 True ]

                    ( _, bytes ) =
                        Text.decodeZString 200 mem
                in
                bytes |> Expect.equal 4
        , test "multi-word string" <|
            \_ ->
                let
                    -- "abcdef" across two words
                    mem =
                        makeMemWithZString 200 [ zword 6 7 8 False, zword 9 10 11 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "abcdef"
        ]



-- ALPHABET TESTS


decodeAlphabetTests : Test
decodeAlphabetTests =
    describe "alphabet characters"
        [ test "all A0 lowercase a-z" <|
            \_ ->
                let
                    -- Build words for all 26 lowercase letters (zchars 6-31)
                    -- Need 9 words (26 chars + padding, 3 per word)
                    allLower =
                        List.range 6 31

                    -- Pad to multiple of 3
                    padded =
                        allLower ++ [ 5, 5 ]

                    words =
                        groupBy3 padded

                    mem =
                        makeMemWithZString 200 words

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "abcdefghijklmnopqrstuvwxyz"
        , test "A2 digits and punctuation (shifted)" <|
            \_ ->
                let
                    -- "0" is A2 zchar 8: shift-5 then 8
                    -- Need: 5, 8, 5  (shift to A2, char 8='0', padding)
                    mem =
                        makeMemWithZString 200 [ zword 5 8 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "0"
        , test "A2 period" <|
            \_ ->
                let
                    -- "." is A2 zchar 18: shift-5 then 18
                    mem =
                        makeMemWithZString 200 [ zword 5 18 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "."
        ]



-- SHIFT TESTS


decodeShiftTests : Test
decodeShiftTests =
    describe "shift characters"
        [ test "shift to A1 for uppercase" <|
            \_ ->
                let
                    -- "A" = shift-4 then zchar 6
                    mem =
                        makeMemWithZString 200 [ zword 4 6 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "A"
        , test "shift is temporary — resets to A0" <|
            \_ ->
                let
                    -- "Ab" = shift-4, zchar 6 (A), zchar 7 (b — back to A0)
                    mem =
                        makeMemWithZString 200 [ zword 4 6 7 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "Ab"
        , test "consecutive shifts" <|
            \_ ->
                let
                    -- "AB" = shift-4, 6(A), shift-4, 6+1 wait...
                    -- Actually: shift-4, 6(A), shift-4, 7(B)
                    -- Needs 2 words: [4, 6, 4] [7, 5, 5]
                    mem =
                        makeMemWithZString 200 [ zword 4 6 4 False, zword 7 5 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "AB"
        , test "mixed case: Hello" <|
            \_ ->
                let
                    -- H=shift4+13, e=10, l=17, l=17, o=20
                    -- [4, 13, 10] [17, 17, 20]
                    mem =
                        makeMemWithZString 200 [ zword 4 13 10 False, zword 17 17 20 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "Hello"
        ]



-- SPECIAL CHARACTERS


decodeSpecialTests : Test
decodeSpecialTests =
    describe "special characters"
        [ test "newline via A2 zchar 7" <|
            \_ ->
                let
                    -- shift-5, zchar 7 = newline
                    mem =
                        makeMemWithZString 200 [ zword 5 7 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "\n"
        , test "10-bit ZSCII escape" <|
            \_ ->
                let
                    -- Character '@' = ZSCII 64 = 0b 00010 00000
                    -- A2 shift(5), escape(6), high 5 bits(2), low 5 bits(0)
                    mem =
                        makeMemWithZString 200 [ zword 5 6 2 False, zword 0 5 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "@"
        , test "10-bit ZSCII escape for tilde ~" <|
            \_ ->
                let
                    -- '~' = ZSCII 126 = 0b 00011 11110
                    -- high = 3, low = 30
                    mem =
                        makeMemWithZString 200 [ zword 5 6 3 False, zword 30 5 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "~"
        , test "padding with zchar 5 produces no output" <|
            \_ ->
                let
                    -- "a" followed by padding (5, 5)
                    mem =
                        makeMemWithZString 200 [ zword 6 5 5 True ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "a"
        ]



-- ABBREVIATION TESTS


abbreviationTests : Test
abbreviationTests =
    describe "abbreviations"
        [ test "abbreviation via zchar 1" <|
            \_ ->
                let
                    -- Abbreviation 1, index 5 → table entry (1-1)*32 + 5 = 5
                    -- Place abbreviation string "hi" at address 220
                    -- "hi" = zchars 13, 14 → [zword 13 14 5 True]
                    abbrStr =
                        [ zword 13 14 5 True ]

                    -- Main string: zchar 1, then 5 (abbreviation index), then padding
                    mem =
                        makeMemWithZStringAndAbbrs 200 [ zword 1 5 5 True ] [ ( 5, 220, abbrStr ) ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "hi"
        , test "abbreviation via zchar 2" <|
            \_ ->
                let
                    -- Abbreviation 2, index 3 → table entry (2-1)*32 + 3 = 35
                    abbrStr =
                        [ zword 20 16 5 True ]

                    -- "ok"
                    mem =
                        makeMemWithZStringAndAbbrs 200 [ zword 2 3 5 True ] [ ( 35, 220, abbrStr ) ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "ok"
        , test "abbreviation via zchar 3" <|
            \_ ->
                let
                    -- Abbreviation 3, index 0 → table entry (3-1)*32 + 0 = 64
                    -- "the" (t=25, h=13, e=10) wait: t=25? t is position 19 in a-z, so zchar = 6+19 = 25. h = 6+7 = 13. e = 6+4 = 10
                    -- Actually: a=6, b=7, c=8 → "cec"? No.
                    -- Let me just use simple chars: zchars 6,7,8 = "abc"
                    abbrStr2 =
                        [ zword 6 7 8 True ]

                    mem =
                        makeMemWithZStringAndAbbrs 200 [ zword 3 0 5 True ] [ ( 64, 220, abbrStr2 ) ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "abc"
        , test "abbreviation embedded in text" <|
            \_ ->
                let
                    -- "x" + abbreviation + "y"
                    -- x=29, abbr 1 idx 0 = entry 0, y=30
                    -- Abbreviation string is "." = shift-5, zchar 18
                    mem =
                        makeMemWithZStringAndAbbrs 200
                            [ zword 29 1 0 False, zword 30 5 5 True ]
                            [ ( 0, 220, [ zword 5 18 5 True ] ) ]

                    ( str, _ ) =
                        Text.decodeZString 200 mem
                in
                str |> Expect.equal "x.y"
        ]



-- ZSCII CONVERSION


zsciiTests : Test
zsciiTests =
    describe "ZSCII conversion"
        [ test "zsciiToChar for printable ASCII" <|
            \_ ->
                Text.zsciiToChar 65 |> Expect.equal 'A'
        , test "zsciiToChar for newline" <|
            \_ ->
                Text.zsciiToChar 13 |> Expect.equal '\n'
        , test "zsciiToChar for space" <|
            \_ ->
                Text.zsciiToChar 32 |> Expect.equal ' '
        , test "zsciiToChar for unprintable returns ?" <|
            \_ ->
                Text.zsciiToChar 0 |> Expect.equal '?'
        , test "charToZscii roundtrip" <|
            \_ ->
                Text.charToZscii 'Z' |> Expect.equal (Just 90)
        , test "charToZscii newline" <|
            \_ ->
                Text.charToZscii '\n' |> Expect.equal (Just 13)
        , test "charToZscii non-ZSCII returns Nothing" <|
            \_ ->
                Text.charToZscii '€' |> Expect.equal Nothing
        ]



-- ENCODING


encodeTests : Test
encodeTests =
    let
        v3mem =
            makeMemWithZString 0x80 []

        v5mem =
            makeV5Mem
    in
    describe "encoding"
        [ test "simple lowercase" <|
            \_ ->
                Text.encodeToZChars v3mem "abc"
                    |> Expect.equal [ 6, 7, 8, 5, 5, 5 ]
        , test "truncates to 6 zchars (V3)" <|
            \_ ->
                Text.encodeToZChars v3mem "abcdefghij"
                    |> Expect.equal [ 6, 7, 8, 9, 10, 11 ]
        , test "pads short strings" <|
            \_ ->
                Text.encodeToZChars v3mem "a"
                    |> Expect.equal [ 6, 5, 5, 5, 5, 5 ]
        , test "uppercase is lowered before encoding" <|
            \_ ->
                Text.encodeToZChars v3mem "ABC"
                    |> Expect.equal [ 6, 7, 8, 5, 5, 5 ]
        , test "empty string is all padding" <|
            \_ ->
                Text.encodeToZChars v3mem ""
                    |> Expect.equal [ 5, 5, 5, 5, 5, 5 ]
        , test "V5 pads to 9 zchars" <|
            \_ ->
                Text.encodeToZChars v5mem "abc"
                    |> Expect.equal [ 6, 7, 8, 5, 5, 5, 5, 5, 5 ]
        , test "V5 truncates to 9 zchars" <|
            \_ ->
                Text.encodeToZChars v5mem "abcdefghijklmnop"
                    |> Expect.equal [ 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
        , test "V5 packs to 3 words" <|
            \_ ->
                Text.packZCharsToWords v5mem (Text.encodeToZChars v5mem "abc")
                    |> List.length
                    |> Expect.equal 3
        , test "V3 packs to 2 words" <|
            \_ ->
                Text.packZCharsToWords v3mem (Text.encodeToZChars v3mem "abc")
                    |> List.length
                    |> Expect.equal 2
        ]



-- decodeZStringWords


decodeZStringWordsTests : Test
decodeZStringWordsTests =
    describe "decodeZStringWords"
        [ test "decodes from word list" <|
            \_ ->
                let
                    -- "abc"
                    mem =
                        makeMemWithZString 200 []

                    str =
                        Text.decodeZStringWords [ zword 6 7 8 True ] mem
                in
                str |> Expect.equal "abc"
        ]



-- HELPERS


groupBy3 : List Int -> List Int
groupBy3 zchars =
    groupBy3Helper zchars []


groupBy3Helper : List Int -> List Int -> List Int
groupBy3Helper remaining acc =
    case remaining of
        a :: b :: c :: rest ->
            let
                isLast =
                    List.isEmpty rest
            in
            groupBy3Helper rest (acc ++ [ zword a b c isLast ])

        [ a, b ] ->
            acc ++ [ zword a b 5 True ]

        [ a ] ->
            acc ++ [ zword a 5 5 True ]

        [] ->
            acc
