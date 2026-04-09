# elm-frotz

A pure Elm Z-Machine version 3 interpreter. Load and run `.z3` interactive fiction files (Zork I, Hitchhiker's Guide, etc.) from any Elm application.

## Quick start

Two imports give you the full API:

```elm
import ZMachine exposing (load, runSteps, provideInput, clearOutput, getOutput)
import ZMachine.Types exposing (StepResult(..), OutputEvent(..), InputRequest(..))
```

Load a story, run it, and handle results:

```elm
case ZMachine.load storyBytes of
    Ok machine ->
        case ZMachine.runSteps 10000 machine of
            Continue nextMachine ->
                -- step budget exhausted, keep going
                ZMachine.runSteps 10000 nextMachine

            NeedInput request machineWithOutput ->
                -- read output, prompt the player, then resume
                let
                    output = ZMachine.getOutput machineWithOutput
                    cleaned = ZMachine.clearOutput machineWithOutput
                in
                -- ... render output, collect input, then:
                ZMachine.provideInput "open mailbox" request cleaned

            Halted machine ->
                -- story finished

            Error err machine ->
                -- something went wrong

    Err reason ->
        -- invalid or unsupported story file
```

Output events are a list you walk through to render:

```elm
ZMachine.getOutput machine
    |> List.map
        (\event ->
            case event of
                PrintText s -> s
                NewLine -> "\n"
                ShowStatusLine status -> status.locationName
                _ -> ""
        )
    |> String.concat
```

## Test harness

The `harness/` directory contains a Node.js runner for trying real `.z3` files interactively:

```bash
cd harness
bash build.sh          # compiles Main.elm -> elm.js
node run.js Zork1.z3   # play!
```

The harness uses Elm ports to shuttle bytes and I/O between Node.js and the interpreter, and yields to the JS event loop between step batches to avoid stack overflow.

## Running tests

```bash
npm test
# or
npx elm-test
```

207 tests across memory, header, instruction decoding, text encoding, execution, and dictionary modules.

## What's implemented

- Full Z-Machine v3 instruction set (all 2OP, 1OP, 0OP, and VAR opcodes)
- Z-string decoding and encoding (alphabets, abbreviations, 10-bit ZSCII escapes)
- Object tree manipulation (attributes, properties, insert/remove)
- Dictionary lookup and input tokenization
- Status line, screen splitting, and cursor control output events
- Random number generation (predictable and random modes)

## What's left

- **Save/restore** -- currently stubs that always fail. Needs Quetzal format support.
- **Compliance testing** -- run against Czech and Praxix test suites.
- **Performance profiling** -- measure and optimise hot paths for large stories.
- **Undo** -- not required by v3 spec but would be nice to have.
- **Sound** -- `PlaySound` events are emitted but there's no reference playback.

## License

MIT
