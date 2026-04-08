module ZMachine.Text exposing
    ( decodeZString
    , decodeZStringWords
    , encodeToZChars
    , zsciiToChar
    , charToZscii
    )

{-| Z-Machine text encoding and decoding.

Handles the conversion between Z-characters (5-bit values packed 3 per word),
ZSCII character codes, and Unicode strings.

@docs decodeZString, decodeZStringWords, encodeToZChars
@docs zsciiToChar, charToZscii

-}

import Bitwise
import ZMachine.Memory as Memory exposing (Memory)


{-| Decode a Z-encoded string starting at a byte address in memory.
Returns the decoded string and the number of bytes consumed.

The abbreviations table address is read from header word 0x18.

-}
decodeZString : Int -> Memory -> ( String, Int )
decodeZString addr mem =
    let
        ( words, bytesConsumed ) =
            readZWords addr mem []

        abbrTableAddr =
            Memory.readWord 0x18 mem

        chars =
            decodeWords words abbrTableAddr mem False
    in
    ( String.fromList chars, bytesConsumed )


{-| Decode a Z-encoded string from a list of pre-read 16-bit words.
Used for inline text in print/print\_ret instructions.
-}
decodeZStringWords : List Int -> Memory -> String
decodeZStringWords words mem =
    let
        abbrTableAddr =
            Memory.readWord 0x18 mem

        chars =
            decodeWords words abbrTableAddr mem False
    in
    String.fromList chars


{-| Encode a string into Z-characters for dictionary lookup.
Returns exactly 6 Z-characters (V3 dictionary format), padded with Z-char 5.
-}
encodeToZChars : String -> List Int
encodeToZChars str =
    let
        zchars =
            str
                |> String.toLower
                |> String.toList
                |> List.concatMap charToZCharSequence
                |> List.take 6

        padded =
            zchars ++ List.repeat (6 - List.length zchars) 5
    in
    padded


{-| Pack Z-characters into 2-byte words for dictionary comparison.
Returns exactly 2 words (V3: 6 Z-chars = 2 words of 3 Z-chars each).
The last word has bit 15 set.
-}
packZCharsToWords : List Int -> List Int
packZCharsToWords zchars =
    let
        padded =
            (zchars ++ List.repeat 6 5) |> List.take 6

        word1 =
            packThreeZChars (List.take 3 padded) False

        word2 =
            packThreeZChars (List.drop 3 padded) True
    in
    [ word1, word2 ]


packThreeZChars : List Int -> Bool -> Int
packThreeZChars chars isLast =
    let
        z1 =
            listGet 0 chars |> Maybe.withDefault 5

        z2 =
            listGet 1 chars |> Maybe.withDefault 5

        z3 =
            listGet 2 chars |> Maybe.withDefault 5

        topBit =
            if isLast then
                0x8000

            else
                0
    in
    topBit
        |> Bitwise.or (Bitwise.shiftLeftBy 10 z1)
        |> Bitwise.or (Bitwise.shiftLeftBy 5 z2)
        |> Bitwise.or z3


{-| Convert a ZSCII code to a Unicode character.
-}
zsciiToChar : Int -> Char
zsciiToChar code =
    if code == 13 then
        '\n'

    else if code >= 32 && code <= 126 then
        Char.fromCode code

    else
        '?'


{-| Convert a Unicode character to a ZSCII code.
Returns Nothing for characters not representable in ZSCII.
-}
charToZscii : Char -> Maybe Int
charToZscii ch =
    let
        code =
            Char.toCode ch
    in
    if ch == '\n' then
        Just 13

    else if code >= 32 && code <= 126 then
        Just code

    else
        Nothing



-- INTERNAL: Reading Z-words from memory


readZWords : Int -> Memory -> List Int -> ( List Int, Int )
readZWords addr mem acc =
    let
        word =
            Memory.readWord addr mem

        newAcc =
            acc ++ [ word ]

        isEnd =
            Bitwise.and word 0x8000 /= 0
    in
    if isEnd then
        ( newAcc, List.length newAcc * 2 )

    else
        readZWords (addr + 2) mem newAcc



-- INTERNAL: Decoding Z-characters from words


type alias DecodeState =
    { output : List Char
    , alphabet : Int
    , pending : Pending
    }


type Pending
    = None
    | Abbreviation Int
    | ZsciiHigh
    | ZsciiLow Int


decodeWords : List Int -> Int -> Memory -> Bool -> List Char
decodeWords words abbrTableAddr mem isAbbreviation =
    let
        zchars =
            words |> List.concatMap extractZChars

        initial =
            { output = [], alphabet = 0, pending = None }

        final =
            List.foldl (processZChar abbrTableAddr mem isAbbreviation) initial zchars
    in
    List.reverse final.output


extractZChars : Int -> List Int
extractZChars word =
    [ Bitwise.and (Bitwise.shiftRightZfBy 10 word) 0x1F
    , Bitwise.and (Bitwise.shiftRightZfBy 5 word) 0x1F
    , Bitwise.and word 0x1F
    ]


processZChar : Int -> Memory -> Bool -> Int -> DecodeState -> DecodeState
processZChar abbrTableAddr mem isAbbreviation zchar state =
    case state.pending of
        Abbreviation abbrBase ->
            let
                abbrIndex =
                    abbrBase + zchar

                abbrWordAddr =
                    abbrTableAddr + abbrIndex * 2

                abbrStringAddr =
                    Memory.readWord abbrWordAddr mem * 2

                abbrChars =
                    if isAbbreviation then
                        -- Abbreviations cannot contain abbreviations
                        []

                    else
                        let
                            ( abbrWords, _ ) =
                                readZWords abbrStringAddr mem []
                        in
                        decodeWords abbrWords abbrTableAddr mem True
            in
            { state
                | output = List.reverse abbrChars ++ state.output
                , pending = None
                , alphabet = 0
            }

        ZsciiHigh ->
            { state | pending = ZsciiLow zchar }

        ZsciiLow highBits ->
            let
                zsciiCode =
                    Bitwise.or (Bitwise.shiftLeftBy 5 highBits) zchar

                ch =
                    zsciiToChar zsciiCode
            in
            { state
                | output = ch :: state.output
                , pending = None
                , alphabet = 0
            }

        None ->
            if zchar == 0 then
                { state | output = ' ' :: state.output, alphabet = 0 }

            else if zchar >= 1 && zchar <= 3 then
                { state | pending = Abbreviation ((zchar - 1) * 32) }

            else if zchar == 4 then
                { state | alphabet = 1 }

            else if zchar == 5 then
                { state | alphabet = 2 }

            else if state.alphabet == 2 && zchar == 6 then
                { state | pending = ZsciiHigh, alphabet = 0 }

            else if state.alphabet == 2 && zchar == 7 then
                { state | output = '\n' :: state.output, alphabet = 0 }

            else
                let
                    ch =
                        alphabetChar state.alphabet zchar
                in
                { state | output = ch :: state.output, alphabet = 0 }



-- INTERNAL: Alphabet tables


{-| V3 default alphabet table. Maps (alphabet, zchar 6-31) to a character.
-}
alphabetChar : Int -> Int -> Char
alphabetChar alphabet zchar =
    let
        index =
            zchar - 6
    in
    case alphabet of
        0 ->
            listGet index a0 |> Maybe.withDefault '?'

        1 ->
            listGet index a1 |> Maybe.withDefault '?'

        2 ->
            listGet index a2 |> Maybe.withDefault '?'

        _ ->
            '?'


a0 : List Char
a0 =
    String.toList "abcdefghijklmnopqrstuvwxyz"


a1 : List Char
a1 =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


a2 : List Char
a2 =
    -- Positions 0-1 (zchars 6-7) are special (10-bit escape and newline),
    -- handled before we reach the lookup. We include placeholders so that
    -- indexing by (zchar - 6) works correctly for zchars 8-31.
    String.toList "^^0123456789.,!?_#'\"/\\-:()"



-- INTERNAL: Encoding characters to Z-chars


charToZCharSequence : Char -> List Int
charToZCharSequence ch =
    case findInAlphabet 0 ch of
        Just zchar ->
            [ zchar ]

        Nothing ->
            case findInAlphabet 1 ch of
                Just zchar ->
                    [ 4, zchar ]

                Nothing ->
                    case findInAlphabet 2 ch of
                        Just zchar ->
                            [ 5, zchar ]

                        Nothing ->
                            -- Fall back to 10-bit ZSCII escape
                            case charToZscii ch of
                                Just code ->
                                    [ 5
                                    , 6
                                    , Bitwise.and (Bitwise.shiftRightZfBy 5 code) 0x1F
                                    , Bitwise.and code 0x1F
                                    ]

                                Nothing ->
                                    []


findInAlphabet : Int -> Char -> Maybe Int
findInAlphabet alphabet ch =
    let
        table =
            case alphabet of
                0 ->
                    a0

                1 ->
                    a1

                _ ->
                    a2
    in
    findIndex ch table
        |> Maybe.map (\i -> i + 6)


findIndex : Char -> List Char -> Maybe Int
findIndex target list =
    findIndexHelper target list 0


findIndexHelper : Char -> List Char -> Int -> Maybe Int
findIndexHelper target list idx =
    case list of
        [] ->
            Nothing

        x :: rest ->
            if x == target then
                Just idx

            else
                findIndexHelper target rest (idx + 1)



-- INTERNAL: Utility


listGet : Int -> List a -> Maybe a
listGet index list =
    list |> List.drop index |> List.head
