module ZMachine.Dictionary exposing (tokenize, lookupWord, TokenResult)

{-| Z-Machine dictionary lookup and tokenization.

Handles splitting input text into words, encoding them for dictionary lookup,
and writing the results to the parse buffer.

@docs tokenize, lookupWord, TokenResult

-}

import Bitwise
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Memory.Header as Header
import ZMachine.Text as Text


{-| Result of tokenizing a single word.
-}
type alias TokenResult =
    { dictAddr : Int
    , textLength : Int
    , textPosition : Int
    }


{-| Tokenize input text and write results to the text buffer and parse buffer.

  - Writes the lowercased input to textBufAddr (starting at byte 1, null-terminated)
  - Splits into words using dictionary separators
  - Looks up each word in the dictionary
  - Writes token results to parseBufAddr

Returns the updated memory.

-}
tokenize : String -> Int -> Int -> Memory -> Memory
tokenize input textBufAddr parseBufAddr mem =
    let
        dictAddr =
            Header.dictionaryAddress mem

        separators =
            readSeparators dictAddr mem

        lowered =
            String.toLower input

        -- Write input to text buffer (byte 1 onward, null-terminated)
        memWithText =
            writeTextBuffer lowered textBufAddr mem

        -- Split into tokens
        tokens =
            splitIntoWords lowered separators

        -- Look up each token in the dictionary
        maxWords =
            Memory.readByte parseBufAddr mem

        tokenResults =
            tokens
                |> List.take maxWords
                |> List.map
                    (\token ->
                        { dictAddr = lookupWord token.text dictAddr mem
                        , textLength = token.length
                        , textPosition = token.position + 1
                        }
                    )
    in
    writeParseBuffer tokenResults parseBufAddr memWithText


{-| Look up a word in the dictionary. Returns the dictionary entry address,
or 0 if not found.
-}
lookupWord : String -> Int -> Memory -> Int
lookupWord word dictAddr mem =
    let
        -- Read dictionary header
        numSeparators =
            Memory.readByte dictAddr mem

        entryLength =
            Memory.readByte (dictAddr + 1 + numSeparators) mem

        numEntries =
            Memory.readSignedWord (dictAddr + 2 + numSeparators) mem

        entriesStart =
            dictAddr + 4 + numSeparators

        -- Encode the word to Z-character dictionary form
        zchars =
            Text.encodeToZChars word

        targetWords =
            Text.packZCharsToWords zchars

        target =
            encodeTargetKey targetWords
    in
    if numEntries >= 0 then
        -- Sorted: binary search
        binarySearch entriesStart entryLength 0 (numEntries - 1) target mem

    else
        -- Unsorted: linear search
        linearSearch entriesStart entryLength (abs numEntries) target mem



-- INTERNAL: Text buffer writing


writeTextBuffer : String -> Int -> Memory -> Memory
writeTextBuffer text addr mem =
    let
        chars =
            String.toList text

        memWithChars =
            List.foldl
                (\( i, ch ) m ->
                    Memory.writeByte (addr + 1 + i) (Char.toCode ch) m
                )
                mem
                (List.indexedMap Tuple.pair chars)
    in
    -- Null-terminate
    Memory.writeByte (addr + 1 + List.length chars) 0 memWithChars



-- INTERNAL: Parse buffer writing


writeParseBuffer : List TokenResult -> Int -> Memory -> Memory
writeParseBuffer results parseBufAddr mem =
    let
        -- Byte 1: number of words found
        memWithCount =
            Memory.writeByte (parseBufAddr + 1) (List.length results) mem
    in
    List.foldl
        (\( i, token ) m ->
            let
                entryAddr =
                    parseBufAddr + 2 + i * 4
            in
            m
                |> Memory.writeWord entryAddr token.dictAddr
                |> Memory.writeByte (entryAddr + 2) token.textLength
                |> Memory.writeByte (entryAddr + 3) token.textPosition
        )
        memWithCount
        (List.indexedMap Tuple.pair results)



-- INTERNAL: Word splitting


type alias Token =
    { text : String
    , position : Int
    , length : Int
    }


splitIntoWords : String -> List Char -> List Token
splitIntoWords input separators =
    splitHelper (String.toList input) separators 0 [] []


splitHelper : List Char -> List Char -> Int -> List Char -> List Token -> List Token
splitHelper chars separators pos currentWord tokens =
    case chars of
        [] ->
            -- Flush any remaining word
            if List.isEmpty currentWord then
                List.reverse tokens

            else
                let
                    word =
                        String.fromList (List.reverse currentWord)

                    wordStart =
                        pos - List.length currentWord
                in
                List.reverse ({ text = word, position = wordStart, length = String.length word } :: tokens)

        ch :: rest ->
            if ch == ' ' then
                -- Space: flush current word, skip the space
                if List.isEmpty currentWord then
                    splitHelper rest separators (pos + 1) [] tokens

                else
                    let
                        word =
                            String.fromList (List.reverse currentWord)

                        wordStart =
                            pos - List.length currentWord
                    in
                    splitHelper rest
                        separators
                        (pos + 1)
                        []
                        ({ text = word, position = wordStart, length = String.length word } :: tokens)

            else if List.member ch separators then
                -- Separator: flush current word, then add separator as its own word
                let
                    flushed =
                        if List.isEmpty currentWord then
                            tokens

                        else
                            let
                                word =
                                    String.fromList (List.reverse currentWord)

                                wordStart =
                                    pos - List.length currentWord
                            in
                            { text = word, position = wordStart, length = String.length word } :: tokens

                    sepToken =
                        { text = String.fromChar ch, position = pos, length = 1 }
                in
                splitHelper rest separators (pos + 1) [] (sepToken :: flushed)

            else
                -- Regular character: accumulate
                splitHelper rest separators (pos + 1) (ch :: currentWord) tokens



-- INTERNAL: Dictionary separator reading


readSeparators : Int -> Memory -> List Char
readSeparators dictAddr mem =
    let
        count =
            Memory.readByte dictAddr mem
    in
    List.range 1 count
        |> List.map (\i -> Char.fromCode (Memory.readByte (dictAddr + i) mem))



-- INTERNAL: Dictionary search


encodeTargetKey : List Int -> Int
encodeTargetKey words =
    case words of
        [ w1, w2 ] ->
            Bitwise.or (Bitwise.shiftLeftBy 16 w1) w2

        _ ->
            0


readEntryKey : Int -> Memory -> Int
readEntryKey addr mem =
    let
        w1 =
            Memory.readWord addr mem

        w2 =
            Memory.readWord (addr + 2) mem
    in
    Bitwise.or (Bitwise.shiftLeftBy 16 w1) w2


binarySearch : Int -> Int -> Int -> Int -> Int -> Memory -> Int
binarySearch entriesStart entryLength low high target mem =
    if low > high then
        0

    else
        let
            mid =
                (low + high) // 2

            midAddr =
                entriesStart + mid * entryLength

            midKey =
                readEntryKey midAddr mem
        in
        if midKey == target then
            midAddr

        else if target < midKey then
            binarySearch entriesStart entryLength low (mid - 1) target mem

        else
            binarySearch entriesStart entryLength (mid + 1) high target mem


linearSearch : Int -> Int -> Int -> Int -> Memory -> Int
linearSearch entriesStart entryLength count target mem =
    linearSearchHelper entriesStart entryLength 0 count target mem


linearSearchHelper : Int -> Int -> Int -> Int -> Int -> Memory -> Int
linearSearchHelper entriesStart entryLength index count target mem =
    if index >= count then
        0

    else
        let
            addr =
                entriesStart + index * entryLength

            key =
                readEntryKey addr mem
        in
        if key == target then
            addr

        else
            linearSearchHelper entriesStart entryLength (index + 1) count target mem
