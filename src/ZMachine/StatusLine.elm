module ZMachine.StatusLine exposing (build)

{-| Assemble the status-line data shown at the top of the screen.

Per Z-Machine Standard §8.2, a V3 status line is derived from the first
three global variables plus `Flags 1` bit 1:

  - Global 0 — the current location object.
  - Global 1 — score (score game) or hours (time game).
  - Global 2 — turns (score game) or minutes (time game).
  - `Flags 1` bit 1 — 1 for a time game, 0 for a score game.

@docs build

-}

import Library.IntExtra exposing (toSignedInt16)
import ZMachine.Header as Header
import ZMachine.Memory as Memory
import ZMachine.ObjectTable as ObjectTable
import ZMachine.Types as Types exposing (ZMachine)


{-| Read the three status globals out of the machine and build a
`StatusLine` record (defined in `ZMachine.Types`).
-}
build : ZMachine -> Types.StatusLine
build machine =
    let
        mem =
            machine.memory

        globalsAddr =
            Header.globalVariablesAddress mem

        locationObj =
            Memory.readWord globalsAddr mem
    in
    { locationName = ObjectTable.shortName locationObj mem
    , score = toSignedInt16 (Memory.readWord (globalsAddr + Memory.wordLength) mem)
    , turns = Memory.readWord (globalsAddr + 2 * Memory.wordLength) mem
    , isTimeGame = Header.testFlag1 Header.StatusLineType mem
    }
