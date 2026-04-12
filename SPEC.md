# elm-frotz: Design and Architecture

**Target:** Z-Machine Standard 1.1, Version 3 story files (.z3)
**Language:** Elm 0.19.1 (package, no ports or kernel code)

---

## Goals

The overarching goal is to make it **easy to build innovative interactive fiction clients** in Elm. A pure, embeddable interpreter means client authors can focus entirely on presentation and interaction design — alternate UIs, accessibility experiments, AI-assisted play, multiplayer overlays, educational tools — without caring about the byte-level details of the Z-Machine.

Concretely:

- **Pure Elm library.** No ports, no JavaScript FFI, no kernel code. The interpreter is a pure function from state + input to state + output. The host application owns all side effects.
- **Correct V3 implementation.** Faithfully implement the Z-Machine Standard 1.1 for version 3 story files. This covers all Infocom-era text adventures: Zork I-III, Hitchhiker's Guide, Planetfall, Enchanter, Deadline, and many others. CZECH 0.8 compliance suite passes 349/349.
- **Structured output.** The library produces typed output events (`PrintText`, `ShowStatusLine`, `SplitWindow`, etc.) rather than rendering anything itself. The host owns all rendering decisions.
- **Correct before fast.** Prioritize spec compliance and code clarity. Performance work comes after a correct, tested baseline.

### Non-goals

- **Versions 4-8** (for now). V3 is a self-contained, well-defined target. See [Open questions](#open-questions) for thoughts on extending.
- **Sound, graphics, or mouse.** V3 has none of these. `PlaySound` events are emitted for the two V3 beeps but there's no reference playback.
- **Debugger/profiler.** Out of scope for the library, though the architecture should make external tooling straightforward.

---

## Architecture decisions

This section records the non-obvious choices that shaped the codebase, with enough rationale that future-us (or a contributor) can judge whether a decision still holds.

### Client interface and responsibility split

The library exposes three modules:

| Module | Purpose |
|--------|---------|
| `ZMachine` | All functions a consumer needs: `load`, `runSteps`, `provideInput`, `provideSaveResult`, `provideRestoreResult`, `snapshot`, `restoreSnapshot` |
| `ZMachine.Types` | All types for pattern matching: `StepResult(..)`, `OutputEvent(..)`, `InputRequest(..)`, `ZMachineError(..)`, `Window(..)`, `Snapshot` |
| `ZMachine.Snapshot` | Snapshot projection, construction, and native encode/decode — for clients building custom save schemes |

Everything else (`Execute`, `Decode`, `Opcode`, `Memory`, `State`, `ObjectTable`, `Dictionary`, `Text`, `Stack`, `StatusLine`, `Header`, `Run`) is internal. Generic helpers live under `Library.*` (`ArrayExtra`, `BytesExtra`, `IntExtra`, `ListExtra`).

**The host is responsible for:**

- Loading the story file (HTTP, file input, whatever) into `Bytes`
- Driving execution by calling `runSteps` with a step budget
- Rendering `OutputEvent` values to a DOM, terminal, or anything else
- Collecting player input and feeding it back via `provideInput`
- Persisting and loading snapshots (localStorage, file download, server, IndexedDB)
- Timing and yielding (animation frames, `Process.sleep 0`, `setImmediate`)

**The library is responsible for:**

- Everything inside the Z-Machine: instruction decode/execute, memory, object tree, text encoding, dictionary lookup, call stack management, random numbers
- Producing structured output events (not rendering)
- Producing and consuming opaque `Snapshot` values (not persisting)
- Rejecting cross-story snapshots on restore

This split means the library has zero dependencies beyond `elm/core` and `elm/bytes`.

### Continuation-based step model

Elm applications cannot run infinite loops. The interpreter uses a **yield-on-event** model: the host calls `runSteps n machine` which executes up to `n` instructions and returns a `StepResult`:

```elm
type StepResult
    = Continue (List OutputEvent) ZMachine
    | NeedInput InputRequest (List OutputEvent) ZMachine
    | NeedSave Snapshot (List OutputEvent) ZMachine
    | NeedRestore (List OutputEvent) ZMachine
    | Halted (List OutputEvent) ZMachine
    | Error ZMachineError (List OutputEvent) ZMachine
```

Every variant carries the `List OutputEvent` accumulated during the call that produced it, plus the machine with its output buffer already cleared. This means the host never needs to manually drain or clear output — it just destructures the variant.

Internally, `Execute.elm` has a private `Outcome` type with the same constructor names but without the output parameter. The 47 construction sites inside `execute` stay output-agnostic. `Run.drain` is the single translation point that reverses `machine.output` (stored newest-first for O(1) append) into chronological order and builds the public `StepResult`.

**Why this two-type design?** The `save` and `restore` opcodes mean Execute already needs `NeedSave`/`NeedRestore` variants — we can't just use `Continue`/`NeedInput`/`Halted`/`Error`. Adding `List OutputEvent` to every variant at every construction site would be pure noise (the output is always "whatever's in machine.output"). Keeping a private Outcome lets Execute focus on instruction semantics while Run handles the output bookkeeping.

**Step budget.** V3 games typically execute a few thousand instructions between input prompts, so a budget of 100,000 usually completes an entire turn in a single batch. The Node.js harness yields via `setImmediate` between batches to avoid stack overflow on long-running sequences. An Elm SPA would use `Process.sleep 0` or `requestAnimationFrame`.

### Memory representation

The Z-Machine's memory is a flat, mutable byte array up to 128KB (V3 max). Elm has no mutable arrays. This is the single most critical design decision in the project.

```elm
type Memory = Memory
    { bytes : Array Int       -- Full memory image, each element 0-255
    , staticBase : Int        -- Byte address where static memory begins
    , fileLength : Int        -- Total file length
    }
```

**Why `Array Int`?** Elm's `Array` is a 32-way-branching relaxed radix balanced (RRB) tree with a tail buffer for the rightmost leaf. For a 128KB image that's ~4 levels deep, giving O(log32 n) reads and functional updates — the best available for Elm's persistent data structures.

**Alternatives considered and rejected:**

| Approach | Problem |
|----------|---------|
| `Dict Int Int` | Higher constant factor, no contiguous-range advantage |
| Flat `Bytes` rebuilt on write | O(n) per write, catastrophically slow |
| `Bytes` with journal of writes | Reads become O(journal size), complex |
| JS `Uint8Array` via ports | Violates "pure Elm" goal |
| Split `Array`/`Bytes` (dynamic/static) | Early version used this; replaced by single Array for simplicity since the overhead is marginal |

Write operations to addresses >= `staticBase` are illegal per spec and indicate a bug in opcode execution. We allow them silently (the memory just accepts the write) rather than adding a branch to every write path. An earlier version split dynamic memory into an `Array` and static memory into read-only `Bytes`, but the complexity of dispatching reads across the boundary wasn't worth the marginal memory saving — V3 files are at most 128KB.

### Snapshot vs Quetzal

There are two serialization formats for save state:

**Native format** (`Snapshot.encode` / `Snapshot.decode`): A straightforward binary dump of the snapshot's parts — dynamic memory, stacks, PC, resume kind, story metadata. Simple to implement, round-trips perfectly, supports both `ResumeAt` (autosave, arbitrary instruction boundary) and `ResumeByBranchTrue` (opcode-driven save). Not portable to other interpreters.

**Quetzal** (IFF `IFZS`): The standard portable format used by Frotz, Bocfel, etc. Uses XOR + run-length encoding for dynamic memory (`CMem` chunk) and a structured call-stack encoding (`Stks` chunk). Quetzal can only represent saves that resume via the `save` opcode's branch — it has no way to express "resume at an arbitrary PC" because the `Stks` format requires the return-PC to point at a store byte.

The library's `Snapshot` type is the internal pivot: both formats serialise from / deserialise to it. The native codec is implemented and tested. Quetzal is parked on a branch (`quetzal-wip`) with encode/decode working but not yet merged.

**Cross-story safety:** `Snapshot.restore` checks release number, serial, and checksum against the story's `originalMemory`. Mismatched snapshots produce a `WrongStory` error; the host can treat this as a failure branch rather than a crash.

### Random number generation

The Z-Machine's `random` opcode needs a PRNG stored in interpreter state. We use a 32-bit xorshift generator (`Library.IntExtra.xorshift`) with shifts 13/17/5, masked to 31 bits to stay within Elm's safe integer range on all platforms.

State is `{ seed : Int, count : Int }`. The `count` field tracks how many values have been generated and serves as a fallback seed when the game calls `random 0` (re-seed to non-deterministic mode). Negative arguments seed with the absolute value for reproducible sequences — games use this in testing and some puzzle mechanics.

**Why not Elm's `Random` module?** `Random.step` requires threading a `Seed` value, which is conceptually the same thing we're already doing with `randomState` in the `ZMachine` record. Using the raw xorshift directly avoids an extra dependency and makes the PRNG's behaviour fully transparent — important for games that depend on reproducible sequences after a negative seed.

### Output accumulation

Output events are stored newest-first in `machine.output` (so `State.appendOutput` is O(1) — a cons onto the head). They are reversed into chronological order at the Run/StepResult boundary. This means:

- Execute doesn't allocate intermediate lists or track output ordering
- The host always sees events in the order they were produced
- The machine returned in each StepResult variant has `output = []`, so no event is delivered twice

An earlier API required the host to call `getOutput` (which reversed) and `clearOutput` (which blanked) manually. This was error-prone: forgetting to clear caused duplicate output, and the two calls were always paired. Bundling output into the StepResult variants eliminated both functions and a class of bugs.

### Error handling

The Z-Machine spec says certain conditions should "halt the interpreter with a suitable error message." We define four error variants:

```elm
type ZMachineError
    = DivisionByZero
    | StackUnderflow
    | InvalidOpcode Int
    | InvalidVariable Int
```

In practice, only two of these are actually raised during execution:

| Condition | What happens | Where |
|-----------|-------------|-------|
| `div` or `mod` by zero | `Error DivisionByZero` | Execute.elm |
| Unrecognised opcode | `Error (InvalidOpcode n)` | Execute.elm (one per opcode class) |

`StackUnderflow` and `InvalidVariable` exist as variants but are **never constructed** anywhere in the codebase. This is the main gap: a large class of spec-illegal conditions are silently absorbed rather than surfaced.

**What gets silently swallowed:**

| Condition | What actually happens | Spec says |
|-----------|----------------------|-----------|
| Read from out-of-bounds memory address | Returns 0 (`Memory.readByte`) | Undefined |
| Write to static/high memory | Silently ignored (`Memory.writeByte`) | Illegal |
| Pop from empty evaluation stack | Returns 0, machine unchanged (`State.popStack`) | Illegal — should be `StackUnderflow` |
| Peek/poke empty stack | Returns 0 / no-op (`State.peekStack`, `pokeStack`) | Illegal |
| Read local variable with no call frame | Returns 0 (`State.readVariable`) | Illegal |
| Write local variable with no call frame | No-op (`State.writeVariable`) | Illegal |
| Read out-of-range local variable | Returns 0 (`Stack.getLocal`) | Illegal |
| Access object 0 (read) | Returns 0 (`ObjectTable.parent/sibling/child`) | 0 = "nothing", reads are sentinel |
| Access object 0 (write) | No-op (`ObjectTable.setParent/setSibling/setChild`) | By convention, no-op is correct |
| `put_prop` on nonexistent property | Silently continues (`Execute.executePutProp`) | Illegal |
| Missing operand in operand list | Returns 0 (`Execute.operandAt`) | Should not happen if decoder is correct |
| Out-of-range Z-character in alphabet | Returns '?' (`Text.alphabetChar`) | Undefined |

**Why it's this way:** The "return a default" approach was chosen for pragmatism during initial development — it lets the interpreter keep running through marginal story-file behaviour rather than halting on every edge case. Well-formed V3 games (Infocom titles, Inform-compiled games) don't trigger these conditions, so the silent defaults have no practical effect on the games we've tested. CZECH passes 349/349 without needing any of them to be errors.

**The trade-off:** If a story file is malformed or a bug is introduced in the interpreter, silent defaults make debugging much harder — the machine happily runs on with garbage state instead of halting at the point of failure. The `StackUnderflow` variant exists precisely because we anticipated wanting to raise it, but never wired it in.

**What "fixing" this would look like:** Thread `Result` or return `Outcome` with `Error` through the low-level operations (`popStack`, `readVariable`, `getLocal`, `readByte`). This is invasive — every call site that reads a variable or touches the stack would need to handle the error case. An alternative is a debug/strict mode flag on `ZMachine` that opts into halting behaviour, keeping the permissive defaults for production use. Neither approach is urgent given that real games don't hit these paths, but it's technical debt worth tracking.

---

## Module structure

```
ZMachine.elm                  -- Public API facade
ZMachine/
  Types.elm                   -- Public types (StepResult, OutputEvent, etc.)
  Snapshot.elm                -- Snapshot capture/restore and native codec
  Run.elm                     -- High-level run loop, Outcome→StepResult translation
  Execute.elm                 -- Opcode dispatch (all V3 opcodes), private Outcome type
  Decode.elm                  -- Instruction decoder (byte stream → Instruction)
  Opcode.elm                  -- Opcode/operand/branch ADTs and classification tables
  Memory.elm                  -- Array-backed byte-addressable memory
  Header.elm                  -- Header field accessors
  State.elm                   -- ZMachine record operations (readVariable, writeVariable, etc.)
  Stack.elm                   -- CallFrame record
  ObjectTable.elm             -- Object tree: attributes, properties, parent/sibling/child
  Text.elm                    -- ZSCII ↔ Unicode, Z-character decode/encode, abbreviations
  Dictionary.elm              -- Dictionary lookup and input tokenization
  StatusLine.elm              -- Status line assembly from global variables
Library/
  ArrayExtra.elm              -- Array.merge (used by Memory.replaceDynamic for restore)
  BytesExtra.elm              -- Bytes utilities
  IntExtra.elm                -- 16-bit arithmetic, xorshift PRNG
  ListExtra.elm               -- getAt and other small helpers
```

### Domain code vs generic helpers

Domain code lives under `ZMachine.*`. Anything that could conceivably exist in a general-purpose Elm utility library lives under `Library.*`. This keeps the Z-Machine modules focused on spec semantics and makes it easy to spot when something is growing too coupled.

---

## Testing strategy

217 tests, all passing. Plus CZECH compliance and a manual Zork I smoke test.

### Unit tests by module

| Module | Tests | What's covered |
|--------|------:|----------------|
| MemoryTest | 36 | Read/write roundtrips, boundary conditions, signed/unsigned, static memory writes |
| HeaderTest | 30 | All V3 header field accessors, flag read/write, interpreter metadata |
| DecodeTest | 55 | Hand-crafted byte sequences → expected `Instruction` records, all 4 forms |
| TextTest | 33 | Z-char sequences → strings, all 3 alphabets, abbreviations, 10-bit escapes, dictionary encoding |
| ExecuteTest | 40 | Arithmetic, branches, control flow, memory access, objects, I/O, errors |
| DictionaryTest | 13 | Lookup, tokenization with separators, parse buffer format |
| SnapshotTest | 10 | Capture/restore roundtrip, native codec roundtrip, cross-story rejection, save/restore opcode integration |

### Compliance testing

**CZECH 0.8** (Comprehensive Z-machine Emulation CHecker by Amir Karger): 349/349 testable opcodes correct, plus all 19 print-opcode visual checks. The story file and reference output live in `testing/compliance/`.

### Integration testing

**Zork I** (Release 119 / Serial 880429) plays interactively through the Node.js harness. The harness uses Elm ports to shuttle bytes and I/O between Node and the interpreter, yielding to the JS event loop between step batches.

### Test helper conventions

Each test file duplicates its own helpers (`makeZM`, `setAt`, `unwrap`, etc.) rather than sharing a common test-support module. This keeps test files self-contained — you can read any one file and understand its setup without chasing imports.

---

## Z-Machine v3 reference

### Instruction decode

The decoder reads from a given PC address and produces an `Instruction`:

1. **Read the opcode byte.** Top 2 bits determine the form:
   - `11` → Variable form, `10` → Short form, otherwise → Long form
2. **Determine operand count** from the form:
   - Short: bits 4-5 = `11` → 0OP; otherwise → 1OP
   - Long: always 2OP
   - Variable: bit 5 = `0` → 2OP; bit 5 = `1` → VAR
3. **Decode operand types and values** (1 byte for small/variable, 2 for large)
4. **Read store variable** if the opcode stores (1 byte)
5. **Read branch data** if the opcode branches (1-2 bytes)
6. **Read inline text** if the opcode is `print` or `print_ret`

### Operand resolution

Before executing, operands are resolved left-to-right. Resolving a `Stack` operand **pops** the stack (destructive read per spec), so order matters when the stack is involved.

### Arithmetic

All arithmetic is signed 16-bit. Results wrap modulo 65536. Division and remainder truncate toward zero. Division by zero is a fatal error (`Error DivisionByZero`).

### Call and return

Routine calls push a `CallFrame` onto the call stack:

```elm
type alias CallFrame =
    { returnPC : Int
    , returnStore : Maybe VariableRef
    , locals : Array Int
    , evalStack : List Int
    }
```

V3 routines declare locals with initial values in the routine header. Provided arguments override leading locals. On return, the frame is popped, the eval stack restored, and the return value stored in `returnStore`.

### Object system

V3: up to 255 objects, each 9 bytes (4 bytes attributes, parent/sibling/child bytes, 2-byte property table address). Properties are in descending order by number, terminated by a zero size byte. `insert_obj` and `remove_obj` maintain the parent/sibling/child linked structure.

### Text encoding

Z-characters are 5-bit values packed 3 per 16-bit word. Three alphabets (lowercase, uppercase, symbols) selected by shift characters 4 and 5. Abbreviations (Z-chars 1-3) expand recursively. 10-bit ZSCII escapes encode characters outside the alphabets. Dictionary lookup encodes input into exactly 6 Z-characters (2 words) with padding.

### Input/output

The `sread` opcode triggers a status-line update then yields `NeedInput`. The host collects a line, and `provideInput` writes it to the text buffer, tokenizes against the dictionary, fills the parse buffer, and returns `Continue`.

Output streams: Stream 1 (screen) is always active. Stream 2 (transcript) is togglable. Stream 3 (memory table redirection) and stream 4 (input script) are minimally handled.

### Save and restore

`save` (0OP:5) yields `NeedSave snapshot machine`. The host persists the snapshot and calls `provideSaveResult True/False`. `restore` (0OP:6) yields `NeedRestore events machine`. The host loads a snapshot and calls `provideRestoreResult (Just snap)` or `Nothing`. Both opcodes use branch-on-success semantics.

---

## Open questions

### Supporting later Z-Machine versions

The games I most want to play are V3, but there are eight versions of the Z-Machine and some important titles are V5+ (Beyond Zork, Trinity, A Mind Forever Voyaging, Bureaucracy, Sherlock). Notes on what extending would involve:

**V4** adds timed input, variable-length call operands (up to 7 args), and more opcodes. The object table grows to 16-bit object numbers. Moderate effort — mostly additive.

**V5** is the big jump: 256 colours, Unicode, `catch`/`throw`, the `extended` opcode form, and the `tokenise`/`encode_text` opcodes. The call frame format changes (locals no longer have initial values in the routine header — they're zeroed). Save/restore switch from branch semantics to store semantics (return a value instead of branching). This is the version that would unlock the most titles.

**V6** adds a graphical window model with proportional fonts, mouse input, and picture display. This is a fundamentally different rendering target and would likely need a separate output-event vocabulary.

**V7-8** are minor variants of V5 with different packed-address multipliers for large story files.

A reasonable path: V5 next (unlocks the most titles with the most manageable scope increase), then V4 as a subset, then V7/V8 as trivial extensions. V6 is a separate project.

The current architecture doesn't prevent this — `Opcode.elm` already has `Unknown*` catch-all constructors, the memory model handles up to 512KB, and the step model is version-agnostic. The main work would be in `Execute.elm` (new opcodes), `Text.elm` (Unicode), and `Snapshot.elm` (V5 save semantics).

### Quetzal format

The native snapshot codec works and round-trips, but isn't portable. Quetzal encode/decode is implemented on the `quetzal-wip` branch but needs: final review, integration with the public API, and test coverage for edge cases (especially `CMem` compression of large dynamic memory regions). Quetzal can only represent `ResumeByBranchTrue` snapshots — autosaves (`ResumeAt`) will always need the native format or a custom scheme.

### Performance

No profiling has been done. The Array-backed memory model gives O(log32 n) per read/write, which should be fine for V3 (typically 16-64KB of dynamic memory, a few thousand memory ops per turn). If profiling reveals hot spots, candidates include: chunked arrays (Array of 256-byte Arrays to reduce structural sharing overhead), caching the globals base address, or batch-decoding instruction sequences.

### Stream 3 (memory table output)

Stream 3 redirects output to a memory table instead of the screen, with up to 16 levels of nesting. It's used by some games for internal string manipulation. Currently minimally handled — the toggle is tracked but output isn't actually redirected to memory. Games that rely on stream 3 (some Inform-compiled games use it for `box` statements) will misbehave.

### Example browser client

The `harness/` directory is a Node.js runner for testing. A proper browser example (Elm SPA with a text area, styled status line, and localStorage save) would make the library much more approachable. This is the obvious next step for demonstrating the "build innovative IF clients" goal.

---

## Key references

- [Z-Machine Standards Document 1.1](https://www.inform-fiction.org/zmachine/standards/z1point1/) — The authoritative specification
- [Z-Machine, And How To Emulate It](http://www.ifarchive.org/if-archive/infocom/interpreters/specification/zspec02/) — Accessible implementation guide by Paul David Doherty
- [Quetzal Save File Format 1.4](https://www.ifarchive.org/if-archive/infocom/interpreters/specification/savefile_14.txt) — Standard portable save format
- [The Inform Designer's Manual](https://www.inform-fiction.org/manual/html/contents.html) — Invaluable for understanding what Inform-compiled games actually do
- [elm/bytes](https://package.elm-lang.org/packages/elm/bytes/latest/) — Binary data in Elm
- [Haskell Z-Machine (zagain)](https://github.com/Nick-Chapman/zagain) — Functional-language reference implementation
- [CZECH compliance suite](https://ifarchive.org/if-archive/infocom/interpreters/tools/czech_0_8.zip) — The test suite we run against
- [Infocom fact sheet](https://www.ifarchive.org/if-archive/infocom/info/fact-sheet.txt) — Which games are which Z-Machine version
- [Community discussion on implementation order](https://intfiction.org/t/process-of-writing-a-z-machine-interpreter/53231) — Practical advice from other implementers

---

## Appendix: V3 opcode reference

### 2OP (two-operand)

| # | Name | St | Br | Description |
|---|------|----|----|-------------|
| 1 | je | | Y | Branch if a == any of b,c,d |
| 2 | jl | | Y | Branch if a < b (signed) |
| 3 | jg | | Y | Branch if a > b (signed) |
| 4 | dec_chk | | Y | Decrement var, branch if < value |
| 5 | inc_chk | | Y | Increment var, branch if > value |
| 6 | jin | | Y | Branch if obj A is child of obj B |
| 7 | test | | Y | Branch if all flags in bitmap set |
| 8 | or | Y | | Bitwise OR |
| 9 | and | Y | | Bitwise AND |
| 10 | test_attr | | Y | Branch if object has attribute |
| 11 | set_attr | | | Set object attribute |
| 12 | clear_attr | | | Clear object attribute |
| 13 | store | | | Write value to variable |
| 14 | insert_obj | | | Move object into another |
| 15 | loadw | Y | | Load word from array |
| 16 | loadb | Y | | Load byte from array |
| 17 | get_prop | Y | | Get object property value |
| 18 | get_prop_addr | Y | | Get property data address |
| 19 | get_next_prop | Y | | Get next property number |
| 20 | add | Y | | Signed 16-bit addition |
| 21 | sub | Y | | Signed 16-bit subtraction |
| 22 | mul | Y | | Signed 16-bit multiplication |
| 23 | div | Y | | Signed 16-bit division |
| 24 | mod | Y | | Signed 16-bit remainder |

### 1OP (one-operand)

| # | Name | St | Br | Description |
|---|------|----|----|-------------|
| 0 | jz | | Y | Branch if value is zero |
| 1 | get_sibling | Y | Y | Get sibling, branch if exists |
| 2 | get_child | Y | Y | Get first child, branch if exists |
| 3 | get_parent | Y | | Get parent object |
| 4 | get_prop_len | Y | | Get property data length |
| 5 | inc | | | Increment variable |
| 6 | dec | | | Decrement variable |
| 7 | print_addr | | | Print string at byte address |
| 9 | remove_obj | | | Detach object from parent |
| 10 | print_obj | | | Print object short name |
| 11 | ret | | | Return value from routine |
| 12 | jump | | | Unconditional jump (signed offset) |
| 13 | print_paddr | | | Print string at packed address |
| 14 | load | Y | | Load value of variable |
| 15 | not | Y | | Bitwise NOT |

### 0OP (zero-operand)

| # | Name | St | Br | Description |
|---|------|----|----|-------------|
| 0 | rtrue | | | Return 1 (true) |
| 1 | rfalse | | | Return 0 (false) |
| 2 | print | | | Print inline Z-string |
| 3 | print_ret | | | Print inline Z-string + newline, return true |
| 4 | nop | | | No operation |
| 5 | save | | Y | Save game state, branch on success |
| 6 | restore | | Y | Restore game state, branch on success |
| 7 | restart | | | Restart the game |
| 8 | ret_popped | | | Return top of stack |
| 9 | pop | | | Discard top of stack |
| 10 | quit | | | Exit game |
| 11 | new_line | | | Print newline |
| 12 | show_status | | | Update status line (V3 only) |
| 13 | verify | | Y | Verify story file checksum |
| 15 | piracy | | Y | Always branch (anti-piracy no-op) |

### VAR (variable-operand)

| # | Name | St | Br | Description |
|---|------|----|----|-------------|
| 0 | call | Y | | Call routine with 0-3 args |
| 1 | storew | | | Store word in array |
| 2 | storeb | | | Store byte in array |
| 3 | put_prop | | | Set object property value |
| 4 | read/sread | | | Read input line, tokenize |
| 5 | print_char | | | Print ZSCII character |
| 6 | print_num | | | Print signed decimal number |
| 7 | random | Y | | Random number or seed |
| 8 | push | | | Push value onto stack |
| 9 | pull | | | Pull value from stack into variable |
| 10 | split_window | | | Set upper window height |
| 11 | set_window | | | Select active window |
| 19 | output_stream | | | Select/deselect output stream |
| 20 | input_stream | | | Select input stream |
| 21 | sound_effect | | | Play a sound (beep/bleep) |
