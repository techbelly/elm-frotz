module ZMachine.Stack exposing (CallFrame, getLocal, setLocal)

{-| Call stack frame management for the Z-Machine.

Each routine call creates a `CallFrame` that captures the return address,
where to store the return value, the routine's local variables, and the
evaluation stack at the point of the call.

@docs CallFrame, getLocal, setLocal

-}

import Array exposing (Array)
import Library.IntExtra exposing (toUnsignedInt16)
import ZMachine.Opcode exposing (VariableRef)


{-| A single call stack frame.
-}
type alias CallFrame =
    { returnPC : Int
    , returnStore : Maybe VariableRef
    , locals : Array Int
    , evalStack : List Int
    , argCount : Int
    }


{-| Read a local variable (1-indexed, as per Z-Machine convention).
Returns 0 for out-of-range indices.
-}
getLocal : Int -> CallFrame -> Int
getLocal n frame =
    Array.get (n - 1) frame.locals |> Maybe.withDefault 0


{-| Write a local variable (1-indexed).
-}
setLocal : Int -> Int -> CallFrame -> CallFrame
setLocal n value frame =
    { frame | locals = Array.set (n - 1) (toUnsignedInt16 value) frame.locals }
