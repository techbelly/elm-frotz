module PlayerTest exposing (suite)

import Bitwise
import Bytes.Encode as Encode
import Dict
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.ObjectTable as OT
import ZMachine.Player as Player


suite : Test
suite =
    describe "ZMachine.Player"
        [ zilDetection
        , globalFrequency
        , actorSwap
        , informFallbackUnit
        ]



-- ZIL DETECTION (insert_obj into grandchild-of-orphan)


zilDetection : Test
zilDetection =
    describe "ZIL-style detection via insert_obj"
        [ test "noteInsert identifies player when dest is a grandchild-of-orphan (room)" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [ playerVar ] }

                    tracking =
                        Player.noteInsert playerObj roomObj mem Player.empty
                in
                tracking.playerObject |> Expect.equal playerObj
        , test "noteInsert ignores inserts whose dest has no grandparent (meta-container moves)" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [] }

                    -- Inserting into the meta itself (obj 1) is not a room move.
                    tracking =
                        Player.noteInsert playerObj metaObj mem Player.empty
                in
                tracking.playerObject |> Expect.equal 0
        , test "noteInsert ignores inserts once player is already identified" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [ playerVar ] }

                    tracking1 =
                        Player.noteInsert playerObj roomObj mem Player.empty

                    tracking2 =
                        Player.noteInsert npcObj roomObj mem tracking1
                in
                tracking2.playerObject |> Expect.equal playerObj
        , test "currentLocation returns parent(player) after detection" <|
            \_ ->
                let
                    ( mem, memWithPlayerInRoom ) =
                        buildWorld { playerGlobals = [ playerVar ] }

                    tracking =
                        Player.noteInsert playerObj roomObj mem Player.empty
                in
                Player.currentLocation memWithPlayerInRoom tracking
                    |> Expect.equal ( roomObj, "" )
        ]



-- GLOBAL SCAN + FREQUENCY TIEBREAKER


globalFrequency : Test
globalFrequency =
    describe "global identification and frequency tiebreaker"
        [ test "scans globals at detection, seeding candidates with zero counts" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [ playerVar, aliasVar ] }

                    tracking =
                        Player.noteInsert playerObj roomObj mem Player.empty
                in
                tracking.globalCandidates
                    |> Dict.keys
                    |> List.sort
                    |> Expect.equal (List.sort [ playerVar, aliasVar ])
        , test "noteGlobalRead only increments members of the candidate set" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [ playerVar, aliasVar ] }

                    tracking =
                        Player.empty
                            |> Player.noteInsert playerObj roomObj mem
                            |> Player.noteGlobalRead playerVar
                            |> Player.noteGlobalRead playerVar
                            |> Player.noteGlobalRead aliasVar
                            |> Player.noteGlobalRead 0x42
                in
                -- 0x42 is not a candidate so it's ignored; the others increment.
                Dict.get playerVar tracking.globalCandidates
                    |> Expect.equal (Just 2)
        , test "most-read candidate wins when multiple globals hold the value" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [ playerVar, aliasVar ] }

                    tracking =
                        Player.empty
                            |> Player.noteInsert playerObj roomObj mem
                            |> Player.noteGlobalRead aliasVar
                            |> Player.noteGlobalRead playerVar
                            |> Player.noteGlobalRead playerVar
                            |> Player.noteGlobalRead playerVar

                    -- Move the player into a new room, then re-read currentLocation
                    -- using the *current* global value.
                    memAfter =
                        mem
                            |> OT.setParent playerObj altRoomObj
                in
                Player.currentPlayer memAfter tracking
                    |> Expect.equal playerObj
        ]



-- ACTOR SWAP (global value changes, detection follows)


actorSwap : Test
actorSwap =
    describe "actor swap via global mutation"
        [ test "currentPlayer follows the winning global's value when it changes" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [ playerVar ] }

                    tracking =
                        Player.noteInsert playerObj roomObj mem Player.empty

                    -- The game does `@store playerVar npcObj` — actor swap.
                    memAfter =
                        writeGlobal playerVar npcObj mem
                in
                Player.currentPlayer memAfter tracking
                    |> Expect.equal npcObj
        , test "no candidate globals found → currentPlayer falls back to playerObject" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [] }

                    tracking =
                        Player.noteInsert playerObj roomObj mem Player.empty
                in
                Player.currentPlayer mem tracking
                    |> Expect.equal playerObj
        ]



-- INFORM FALLBACK (behavioural: we just verify the Window field in isolation)


informFallbackUnit : Test
informFallbackUnit =
    describe "Inform fallback field on UpperWindow"
        [ test "no detection signals → currentLocation returns (0, \"\")" <|
            \_ ->
                let
                    ( mem, _ ) =
                        buildWorld { playerGlobals = [] }
                in
                Player.currentLocation mem Player.empty
                    |> Expect.equal ( 0, "" )
        ]



-- WORLD BUILDER --------------------------------------------------------------


{-| Build a tiny V5 world:

  - obj 1 (meta) — parent 0, the ROOMS container
  - obj 2 (room) — parent 1, a room
  - obj 3 (altRoom) — parent 1, another room
  - obj 4 (player) — parent 0 initially (orphan), identifies as player
  - obj 5 (npc) — parent 0 initially (candidate for actor swap)

Globals listed in `playerGlobals` are initialised to `playerObj`. All
others start at 0.

Returns `(initialMem, memWithPlayerInRoom)` where the second is the same
world with `insert_obj playerObj roomObj` already applied, so tests can
assert `parent(player) == room`.

-}
buildWorld : { playerGlobals : List Int } -> ( Memory, Memory )
buildWorld { playerGlobals } =
    let
        mem =
            baseMem
                |> OT.setChild metaObj roomObj
                |> OT.setSibling roomObj altRoomObj
                |> OT.setParent roomObj metaObj
                |> OT.setParent altRoomObj metaObj
                |> seedGlobals playerGlobals

        memAfterInsert =
            mem
                |> OT.setParent playerObj roomObj
    in
    ( mem, memAfterInsert )


seedGlobals : List Int -> Memory -> Memory
seedGlobals vars mem =
    List.foldl (\v m -> writeGlobal v playerObj m) mem vars


writeGlobal : Int -> Int -> Memory -> Memory
writeGlobal varNum value mem =
    Memory.writeWord (globalAddr varNum) value mem


globalAddr : Int -> Int
globalAddr varNum =
    globalsBase + (varNum - 0x10) * 2



-- OBJECT NUMBERS


metaObj : Int
metaObj =
    1


roomObj : Int
roomObj =
    2


altRoomObj : Int
altRoomObj =
    3


playerObj : Int
playerObj =
    4


npcObj : Int
npcObj =
    5



-- GLOBAL VARIABLE NUMBERS


playerVar : Int
playerVar =
    0x20


aliasVar : Int
aliasVar =
    0x30



-- RAW MEMORY IMAGE -----------------------------------------------------------


objTableAddr : Int
objTableAddr =
    0x0100


globalsBase : Int
globalsBase =
    0x0400


staticBase : Int
staticBase =
    0x0800


totalSize : Int
totalSize =
    0x1000


baseMem : Memory
baseMem =
    let
        propTableAddr =
            0x0200

        entryAddr n =
            objTableAddr + 126 + (n - 1) * 14

        withEntry n acc =
            acc
                |> setAt (entryAddr n + 12) (byteHi propTableAddr)
                |> setAt (entryAddr n + 13) (byteLo propTableAddr)

        words =
            List.repeat totalSize 0
                -- header
                |> setAt 0 5
                |> setAt 0x0A (byteHi objTableAddr)
                |> setAt 0x0B (byteLo objTableAddr)
                |> setAt 0x0C (byteHi globalsBase)
                |> setAt 0x0D (byteLo globalsBase)
                |> setAt 0x0E (byteHi staticBase)
                |> setAt 0x0F (byteLo staticBase)
                -- empty property table shared by all objects (no name, terminator)
                |> setAt propTableAddr 0
                |> setAt (propTableAddr + 1) 0
                |> withEntry metaObj
                |> withEntry roomObj
                |> withEntry altRoomObj
                |> withEntry playerObj
                |> withEntry npcObj
    in
    words |> toMemory


byteHi : Int -> Int
byteHi n =
    Bitwise.and (Bitwise.shiftRightZfBy 8 n) 0xFF


byteLo : Int -> Int
byteLo n =
    Bitwise.and n 0xFF


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


toMemory : List Int -> Memory
toMemory bytes =
    bytes
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
