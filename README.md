# elm-zmachine

A pure Elm Z-Machine interpreter. Load and run `.z3` and `.z5` interactive fiction files (Zork I, Hitchhiker's Guide, and the wider Infocom catalogue) from any Elm application. Version 3 is the primary target; version 5 is partial (see below).

## Quick start

Two imports give you the full API:

```elm
import ZMachine exposing (load, runSteps, provideInput)
import ZMachine.Types exposing (StepResult(..), OutputEvent(..))
```

Load a story, run it, and handle results. Every `StepResult` variant
carries the output events produced during the call, so you never need
to reach into the machine's output buffer:

```elm
case ZMachine.load storyBytes of
    Ok machine ->
        case ZMachine.runSteps 10000 machine of
            Continue events nextMachine ->
                -- step budget exhausted; render events, then keep going
                ZMachine.runSteps 10000 nextMachine

            NeedInput info events next ->
                -- render events, prompt the player, then resume
                ZMachine.provideInput "open mailbox" info next

            NeedChar events next ->
                -- prompt for a single keypress, then resume
                ZMachine.provideChar "y" next

            Halted events final ->
                -- story finished

            Error err events final ->
                -- something went wrong

            NeedSave snap events next ->
                -- persist `snap`, then ZMachine.provideSaveResult True next
                ...

            NeedRestore events next ->
                -- load a snapshot, then ZMachine.provideRestoreResult (Just snap) next
                ...

    Err reason ->
        -- invalid or unsupported story file
```

Output events are a list you walk through to render:

```elm
events
    |> List.map
        (\event ->
            case event of
                PrintText s -> s
                ShowStatusLine status -> status.locationName
                _ -> ""
        )
    |> String.concat
```

## What's implemented

- Full Z-Machine v3 instruction set (all 2OP, 1OP, 0OP, and VAR opcodes), including correct in-place semantics for indirect variable references to the stack pointer per spec §6.3.4
- Version-5 extensions: the v5 opcode set, extended decoding, version-aware text encoding, and a virtual upper window with stream 3 support
- Z-string decoding and encoding (alphabets, abbreviations, 10-bit ZSCII escapes)
- Object tree manipulation (attributes, properties, insert/remove)
- Dictionary lookup and input tokenization
- Status line, screen splitting, and cursor control output events
- Random number generation (predictable and random modes)
- Save and restore: the `save` / `restore` opcodes surface as `NeedSave` / `NeedRestore` step results carrying a host-persistable `Snapshot`, plus host-driven `ZMachine.snapshot` / `ZMachine.restoreSnapshot` for autosaves. See [`ZMachine.Snapshot`](src/ZMachine/Snapshot.elm) for the native encode/decode codec; cross-story snapshots are rejected by release / serial / checksum
- Passes the CZECH v3 compliance suite (349/349) and the CZECH v5 suite (406/406); plays Zork I end-to-end

## What's partially implemented or missing

- **Version 5 polish** — v5 story files load, run, and pass the CZECH v5 suite, but the richer v5 screen model (font 3 glyphs, mouse input, the full colour palette) is surfaced as output events rather than rendered faithfully. Games that lean heavily on those features may not look identical to reference interpreters.
- **Versions 4, 6, 7, 8** — only v3 and v5 load. Other versions are rejected at load time.
- **Quetzal save-file format** — the save/restore machinery is wired up through `Snapshot`, but the standard portable Quetzal (IFF) codec is still parked on a branch. The built-in `Snapshot.encode` / `Snapshot.decode` is a native format that round-trips but isn't interchangeable with other interpreters.
- **Sound** — `PlaySound` events are emitted but there's no reference playback.

## Contributing and internals

See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the test harness, how to run the unit tests and the CZECH compliance suite, and notes on the bundled story file.

## License

MIT
