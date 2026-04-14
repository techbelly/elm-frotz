module ZMachine.Player exposing
    ( empty
    , noteInsert
    , noteGlobalRead
    , currentPlayer
    , currentLocation
    )

{-| Identify which Z-machine global variable holds the "current player"
object, so we can report `locationId = parent(player)` for V5 games.

V5 has no spec convention for the player object. Games supply their own.
We infer it dynamically, in two steps:

  1. **Structural detection.** The first time `insert_obj X Y` fires
     where `Y` is a room (grandchild of an orphan — the `ROOMS`
     meta-container pattern), `X` is almost certainly the player being
     placed in the starting room by the init routine.

  2. **Global identification.** Scan globals for any whose value equals
     `X`. That global is the player reference. If multiple globals
     match, use read-frequency as a tiebreaker — the player global is
     read more often than an arbitrary alias.

Once we know which global holds the player, we read it fresh each time
we need `locationId`, so `ChangePlayer()`-style actor swaps are followed
automatically.

-}

import Dict exposing (Dict)
import ZMachine.Header as Header
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.ObjectTable as ObjectTable


{-| Initial tracking state: no player detected, no candidates.
-}
empty : { playerObject : Int, globalCandidates : Dict Int Int }
empty =
    { playerObject = 0, globalCandidates = Dict.empty }


{-| Called after every `insert_obj obj dest`. If this looks like the
engine placing the player in the starting room and we haven't detected
a player yet, identify `obj` as the player and seed the candidate set
with every global currently holding that value.
-}
noteInsert :
    Int
    -> Int
    -> Memory
    -> { a | playerObject : Int, globalCandidates : Dict Int Int }
    -> { a | playerObject : Int, globalCandidates : Dict Int Int }
noteInsert obj dest mem tracking =
    if tracking.playerObject /= 0 then
        tracking

    else if isRoom dest mem then
        { tracking
            | playerObject = obj
            , globalCandidates = scanGlobalsForValue obj mem
        }

    else
        tracking


{-| A room is a direct child of an orphan object (the ZIL `ROOMS`
meta-container pattern). Equivalently: `parent(dest) /= 0` and
`parent(parent(dest)) == 0`.
-}
isRoom : Int -> Memory -> Bool
isRoom objNum mem =
    let
        p =
            ObjectTable.parent objNum mem
    in
    p /= 0 && ObjectTable.parent p mem == 0


{-| Called whenever a global is read. If it's a candidate for the
player reference, bump its read count (used as a tiebreaker when
multiple globals hold the player's value).
-}
noteGlobalRead :
    Int
    -> { a | globalCandidates : Dict Int Int }
    -> { a | globalCandidates : Dict Int Int }
noteGlobalRead varNum tracking =
    if Dict.member varNum tracking.globalCandidates then
        { tracking
            | globalCandidates =
                Dict.update varNum (Maybe.map ((+) 1)) tracking.globalCandidates
        }

    else
        tracking


{-| Best guess at the current player object. Reads the winning global if
one exists, so actor swaps (`@store playerGlobal newPlayer`) are
reflected automatically. Falls back to the last directly-detected
object when no global matched, or 0 if we've detected nothing yet.
-}
currentPlayer :
    Memory
    -> { a | playerObject : Int, globalCandidates : Dict Int Int }
    -> Int
currentPlayer mem tracking =
    case bestCandidate tracking.globalCandidates of
        Just globalN ->
            readGlobal globalN mem

        Nothing ->
            tracking.playerObject


{-| `(locationId, locationName)` for the current player. Returns
`(0, "")` when no player has been identified.
-}
currentLocation :
    Memory
    -> { a | playerObject : Int, globalCandidates : Dict Int Int }
    -> ( Int, String )
currentLocation mem tracking =
    let
        player =
            currentPlayer mem tracking
    in
    if player == 0 then
        ( 0, "" )

    else
        let
            loc =
                ObjectTable.parent player mem
        in
        ( loc, ObjectTable.shortName loc mem )


{-| Global with the highest read count. Ties broken by lower variable
number (deterministic since `Dict.toList` is sorted by key and
`List.sortBy` is stable).
-}
bestCandidate : Dict Int Int -> Maybe Int
bestCandidate candidates =
    Dict.toList candidates
        |> List.sortBy (\( _, count ) -> -count)
        |> List.head
        |> Maybe.map Tuple.first


scanGlobalsForValue : Int -> Memory -> Dict Int Int
scanGlobalsForValue target mem =
    List.range firstGlobalVar lastGlobalVar
        |> List.filterMap
            (\n ->
                if readGlobal n mem == target then
                    Just ( n, 0 )

                else
                    Nothing
            )
        |> Dict.fromList


readGlobal : Int -> Memory -> Int
readGlobal varNum mem =
    Memory.readWord
        (Header.globalVariablesAddress mem + (varNum - firstGlobalVar) * Memory.wordLength)
        mem


firstGlobalVar : Int
firstGlobalVar =
    0x10


lastGlobalVar : Int
lastGlobalVar =
    0xFF
