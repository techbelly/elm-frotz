# elm-zmachine

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

The `harness/` directory contains a Node.js runner for trying real `.z3` files interactively. All of the usual tasks are wired up as npm scripts:

```bash
npm run build            # type-check the library
npm run build-harness    # compile harness/Main.elm -> harness/elm.js
npm run run-harness -- testing/Zork1.z3   # play!
```

The harness uses Elm ports to shuttle bytes and I/O between Node.js and the interpreter, and yields to the JS event loop between step batches to avoid stack overflow.

### About the bundled Zork I story file

`testing/Zork1.z3` is Zork I Release 119 / Serial 880429, copied unchanged from the MIT-licensed [historicalsource/zork1](https://github.com/historicalsource/zork1) repository (© 2025 Microsoft, MIT License). It's included so the harness has a real story file to run out of the box. The full Infocom source code for the game is available in that repository for anyone wanting to study or rebuild it.

## Running tests

```bash
npm test           # elm-test unit tests + CZECH compliance run
npm run test-elm   # just the unit tests
npm run test-czech # just the CZECH compliance run
```

**Unit tests:**  tests across memory, header, instruction decoding, text encoding, execution, and dictionary modules.

**Compliance:** CZECH 0.8 (Comprehensive Z-machine Emulation CHecker by Amir Karger) passes with 349 of 349 testable opcodes correct, plus all 19 print-opcode visual checks — matching the reference output for a spec-compliant interpreter. The story file and expected reference output live in `testing/compliance/`.

## What's implemented

- Full Z-Machine v3 instruction set (all 2OP, 1OP, 0OP, and VAR opcodes), including correct in-place semantics for indirect variable references to the stack pointer per spec §6.3.4
- Z-string decoding and encoding (alphabets, abbreviations, 10-bit ZSCII escapes)
- Object tree manipulation (attributes, properties, insert/remove)
- Dictionary lookup and input tokenization
- Status line, screen splitting, and cursor control output events
- Random number generation (predictable and random modes)
- Passes the CZECH v3 compliance suite (349/349) and plays Zork I end-to-end

## What's not implemented yet

- **Save/restore** -- currently stubs that always fail. Needs Quetzal format support.
- **Performance profiling** -- measure and optimise hot paths for large stories.
- **Undo** -- not required by v3 spec but would be nice to have.
- **Sound** -- `PlaySound` events are emitted but there's no reference playback.

## License

MIT
