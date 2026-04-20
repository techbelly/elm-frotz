# Contributing to elm-zmachine

This document covers the local development setup — the test harness,
how to run the unit tests and the CZECH compliance suite, and some
notes on the bundled Zork I story file that make the harness usable
out of the box.

## Test harness

The `harness/` directory contains a Node.js runner for trying real `.z3` files interactively. All of the usual tasks are wired up as npm scripts:

```bash
npm run build            # type-check the library
npm run build-harness    # compile harness/Main.elm -> harness/dist/elm.js
npm run run-harness -- testing/Zork1.z3   # play!
```

The harness uses Elm ports to shuttle bytes and I/O between Node.js and the interpreter, and yields to the JS event loop between step batches to avoid stack overflow. It does not yet wire up file-backed save/restore — stories that hit a `save` or `restore` opcode see the operation reported as failure, and the story's branch is taken accordingly.

## Running tests

```bash
npm test           # elm-test unit tests + CZECH compliance run
npm run test-elm   # just the unit tests
npm run test-czech # just the CZECH compliance run
```

**Unit tests:** 281 tests across memory, header, instruction decoding, text encoding, execution, and dictionary modules.

**Compliance:** CZECH 0.8 (Comprehensive Z-machine Emulation CHecker by Amir Karger) passes with 349 of 349 testable opcodes correct, plus all 19 print-opcode visual checks — matching the reference output for a spec-compliant interpreter. The story file and expected reference output live in `testing/compliance/`.

## About the bundled Zork I story file

`testing/Zork1.z3` is Zork I Release 119 / Serial 880429, copied unchanged from the MIT-licensed [historicalsource/zork1](https://github.com/historicalsource/zork1) repository (© 2025 Microsoft, MIT License). It's included so the harness has a real story file to run out of the box. The full Infocom source code for the game is available in that repository for anyone wanting to study or rebuild it.
