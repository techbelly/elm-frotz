module ZMachine.Window exposing
    ( empty
    , printText
    , newLine
    , split
    , setCursor
    , getCursor
    , eraseWindow
    , eraseLine
    , notePrintObj
    , render
    )

{-| Virtual upper window for V5+ games.

V5 games draw their own status line by writing directly to the upper
window with cursor positioning. This module maintains the character
grid and cursor state, and can render the final content as a list of
trimmed strings for inclusion in a `ShowStatusLine` event.

@docs empty
@docs printText, newLine
@docs split, setCursor, getCursor
@docs eraseWindow, eraseLine
@docs render

-}

import Array exposing (Array)
import Dict
import ZMachine.Types exposing (UpperWindow)


{-| An empty upper window of the given width.
-}
empty : Int -> UpperWindow
empty width =
    { height = 0
    , rows = Dict.empty
    , cursorRow = 1
    , cursorCol = 1
    , width = width
    , firstPrintedObj = 0
    }


{-| Write text at the current cursor position, advancing the cursor.
-}
printText : String -> UpperWindow -> UpperWindow
printText str uw =
    let
        chars =
            String.toList str

        row =
            Dict.get uw.cursorRow uw.rows
                |> Maybe.withDefault (Array.repeat uw.width ' ')

        ( newCol, updatedRow ) =
            List.foldl
                (\ch ( col, arr ) ->
                    if col < Array.length arr then
                        ( col + 1, Array.set col ch arr )

                    else
                        ( col + 1, arr )
                )
                ( uw.cursorCol - 1, row )
                chars
    in
    { uw
        | rows = Dict.insert uw.cursorRow updatedRow uw.rows
        , cursorCol = newCol + 1
    }


{-| Advance to the next line.
-}
newLine : UpperWindow -> UpperWindow
newLine uw =
    { uw | cursorRow = uw.cursorRow + 1, cursorCol = 1 }


{-| Split the screen, setting the upper window height.
Resets the cursor to (1,1) per the V5 spec.
-}
split : Int -> UpperWindow -> UpperWindow
split height uw =
    { uw
        | height = height
        , cursorRow = 1
        , cursorCol = 1
        , firstPrintedObj = 0
    }


{-| Set the cursor position (1-based row and column).
-}
setCursor : Int -> Int -> UpperWindow -> UpperWindow
setCursor row col uw =
    { uw | cursorRow = row, cursorCol = col }


{-| Get the current cursor position as (row, col), 1-based.
-}
getCursor : UpperWindow -> ( Int, Int )
getCursor uw =
    ( uw.cursorRow, uw.cursorCol )


{-| Erase a window. Values 1, -1, and -2 clear the upper window grid
and reset the cursor. Value -1 also resets the height to 0 (unsplit).
-}
eraseWindow : Int -> UpperWindow -> UpperWindow
eraseWindow win uw =
    if win == 1 || win == -1 || win == -2 then
        let
            cleared =
                { uw
                    | rows = Dict.empty
                    , cursorRow = 1
                    , cursorCol = 1
                    , firstPrintedObj = 0
                }
        in
        if win == -1 then
            { cleared | height = 0 }

        else
            cleared

    else
        uw


{-| Record that `print_obj <n>` was executed while the upper window was
current. Inform-compiled games draw the status bar by printing the
location object's short name first; capturing that object number lets
the interpreter identify the current room without tracking the player.
Subsequent `print_obj` calls within the same draw are ignored.
-}
notePrintObj : Int -> UpperWindow -> UpperWindow
notePrintObj objNum uw =
    if uw.firstPrintedObj == 0 then
        { uw | firstPrintedObj = objNum }

    else
        uw


{-| Erase from the cursor to the end of the current line (when value
is 1). Fills with spaces.
-}
eraseLine : Int -> UpperWindow -> UpperWindow
eraseLine value uw =
    if value == 1 then
        let
            row =
                Dict.get uw.cursorRow uw.rows
                    |> Maybe.withDefault (Array.repeat uw.width ' ')

            cleared =
                List.range (uw.cursorCol - 1) (uw.width - 1)
                    |> List.foldl (\col arr -> Array.set col ' ' arr) row
        in
        { uw | rows = Dict.insert uw.cursorRow cleared uw.rows }

    else
        uw


{-| Render the upper window rows as a list of trimmed strings.
Returns an empty list if the window has no visible content.
-}
render : UpperWindow -> List String
render uw =
    List.range 1 (max uw.height 1)
        |> List.filterMap
            (\r ->
                Dict.get r uw.rows
                    |> Maybe.map renderRow
            )
        |> List.filter (not << String.isEmpty)


{-| Render a single row, splitting on runs of 3+ spaces to cleanly
separate cursor-positioned sections (location name, score, etc.).
-}
renderRow : Array Char -> String
renderRow row =
    let
        full =
            String.fromList (Array.toList row)
                |> String.trim
    in
    if String.isEmpty full then
        ""

    else
        splitOnSpaces full
            |> String.join "  "


splitOnSpaces : String -> List String
splitOnSpaces str =
    splitOnSpacesHelp (String.toList str) [] [] False 0


splitOnSpacesHelp : List Char -> List Char -> List String -> Bool -> Int -> List String
splitOnSpacesHelp chars current segments inGap gapLen =
    case chars of
        [] ->
            let
                final =
                    String.fromList (List.reverse current) |> String.trim
            in
            List.reverse
                (if String.isEmpty final then
                    segments

                 else
                    final :: segments
                )

        ' ' :: rest ->
            if inGap then
                splitOnSpacesHelp rest current segments True (gapLen + 1)

            else
                splitOnSpacesHelp rest (' ' :: current) segments True 1

        ch :: rest ->
            if inGap && gapLen >= 3 then
                let
                    seg =
                        String.fromList (List.reverse current) |> String.trim
                in
                splitOnSpacesHelp rest [ ch ]
                    (if String.isEmpty seg then
                        segments

                     else
                        seg :: segments
                    )
                    False
                    0

            else
                splitOnSpacesHelp rest (ch :: current) segments False 0
