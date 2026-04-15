#!/usr/bin/env node
/*
 * Z-machine interpreter micro-benchmark.
 *
 * Runs one or more story workloads through the interpreter and reports
 * timing (min/median/mean/max over N measured runs) plus process memory.
 * Designed to be re-run before and after interpreter changes so we can
 * track the impact on real workloads.
 *
 * Timing uses process.hrtime.bigint() around the interpreter loop only,
 * so Node startup, file I/O, and Elm runtime init are excluded from the
 * reported numbers.
 *
 * Usage:
 *   npm run bench                                             # default suite
 *   node --expose-gc harness/bench.js                         # default suite (direct)
 *   node --expose-gc harness/bench.js --story testing/Zork1.z3 --input testing/bench/zork1.inputs
 *   node --expose-gc harness/bench.js --runs 10 --warmup 2
 *
 * Interpreting the numbers:
 *   - Compare `min` or `median` across runs. `mean` and `max` are noisy
 *     because of GC pauses and V8 tier-up on the first measured iteration.
 *   - `heapUsed` is the signal for interpreter memory cost. `rss` is what
 *     Node has mapped from the OS; it can balloon to 100+ MB without any
 *     real leak and is noisy between runs.
 *   - Always capture "before" and "after" numbers with the same script
 *     version, the same elm.js build flags, and the same Node version.
 *
 * Current baseline (DEV-mode elm.js, Node v20+, --expose-gc, 1 warmup + 5 runs):
 *
 *   CZECH compliance
 *     timing    min ~10 ms   median ~11 ms
 *     memory    heapUsed +1.1 MB
 *
 *   Zork I walkthrough (66 commands)
 *     timing    min ~77 ms   median ~81 ms
 *     memory    heapUsed +1.9 MB
 *
 *   Captured before the Memory.elm static-region refactor.
 */

'use strict';

const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '..');

// Elm's DEV-mode bundle prints a warning to console.warn on module load.
// Silence it so the bench output stays clean.
const _origWarn = console.warn;
console.warn = () => {};
const { Elm } = require('./dist/elm.js');
console.warn = _origWarn;

const DEFAULT_WORKLOADS = [
    {
        name: 'CZECH compliance',
        story: 'testing/compliance/czech.z3',
        input: null,
    },
    {
        name: 'Zork I walkthrough',
        story: 'testing/Zork1.z3',
        input: 'testing/bench/zork1.inputs',
    },
];

function parseArgs(argv) {
    const args = { runs: 5, warmup: 1, story: null, input: null };
    for (let i = 2; i < argv.length; i++) {
        const a = argv[i];
        switch (a) {
            case '--runs':
                args.runs = parseInt(argv[++i], 10);
                break;
            case '--warmup':
                args.warmup = parseInt(argv[++i], 10);
                break;
            case '--story':
                args.story = argv[++i];
                break;
            case '--input':
                args.input = argv[++i];
                break;
            case '-h':
            case '--help':
                console.error(
                    'Usage: node --expose-gc harness/bench.js ' +
                        '[--story PATH] [--input PATH] [--runs N] [--warmup N]'
                );
                process.exit(0);
                break;
            default:
                console.error('unknown arg: ' + a);
                process.exit(2);
        }
    }
    return args;
}

function loadWorkload(spec) {
    const storyBytes = fs.readFileSync(path.resolve(ROOT, spec.story));
    const cmds = spec.input
        ? fs
              .readFileSync(path.resolve(ROOT, spec.input), 'utf8')
              .split('\n')
              .filter((l) => l.length > 0)
        : [];
    return {
        name: spec.name || path.basename(spec.story),
        storyBytes,
        cmds,
    };
}

function runOnce(workload) {
    return new Promise((resolve, reject) => {
        const app = Elm.Main.init();
        let cmdIdx = 0;
        let inputCount = 0;
        let t0;
        let finished = false;

        const finish = (result) => {
            if (finished) return;
            finished = true;
            resolve(result);
        };

        app.ports.output.subscribe(() => {
            /* discard */
        });

        app.ports.requestInput.subscribe((signal) => {
            if (signal === 'HALT' || cmdIdx >= workload.cmds.length) {
                const elapsedMs = Number(process.hrtime.bigint() - t0) / 1e6;
                finish({
                    elapsedMs,
                    inputCount,
                    halted: signal === 'HALT',
                });
                return;
            }
            inputCount++;
            const cmd = workload.cmds[cmdIdx++];
            setImmediate(() => app.ports.inputProvided.send(cmd));
        });

        app.ports.continueRunning.subscribe(() =>
            setImmediate(() => app.ports.resume.send(null))
        );

        app.ports.errorOccurred.subscribe((e) => {
            if (finished) return;
            finished = true;
            reject(new Error(String(e)));
        });

        t0 = process.hrtime.bigint();
        app.ports.storyLoaded.send(Array.from(workload.storyBytes));
    });
}

async function benchmark(workload, runs, warmup) {
    for (let i = 0; i < warmup; i++) {
        await runOnce(workload);
    }
    const times = [];
    let last = null;
    for (let i = 0; i < runs; i++) {
        last = await runOnce(workload);
        times.push(last.elapsedMs);
    }
    const sorted = times.slice().sort((a, b) => a - b);
    return {
        min: sorted[0],
        max: sorted[sorted.length - 1],
        mean: times.reduce((a, b) => a + b, 0) / times.length,
        median: sorted[Math.floor(sorted.length / 2)],
        runs: times.length,
        inputs: last.inputCount,
        halted: last.halted,
    };
}

function fmtMs(n) {
    return n.toFixed(2) + ' ms';
}

function fmtMB(n) {
    return (n / (1024 * 1024)).toFixed(1) + ' MB';
}

function fmtDeltaMB(n) {
    const sign = n >= 0 ? '+' : '';
    return sign + (n / (1024 * 1024)).toFixed(1) + ' MB';
}

function snapshotMemory() {
    if (global.gc) global.gc();
    const m = process.memoryUsage();
    return { rss: m.rss, heapUsed: m.heapUsed, heapTotal: m.heapTotal };
}

async function main() {
    const args = parseArgs(process.argv);

    const specs = args.story
        ? [
              {
                  name: path.basename(args.story),
                  story: args.story,
                  input: args.input,
              },
          ]
        : DEFAULT_WORKLOADS;

    const workloads = specs.map(loadWorkload);

    console.error(
        'elm-frotz bench  (' +
            args.warmup +
            ' warmup + ' +
            args.runs +
            ' measured runs per workload)'
    );
    console.error(
        global.gc
            ? '(memory measured after forced GC)'
            : '(hint: rerun with `node --expose-gc` for GC-stabilised memory)'
    );
    console.error('');

    const baseline = snapshotMemory();
    console.error(
        'baseline memory: rss=' +
            fmtMB(baseline.rss) +
            '  heapUsed=' +
            fmtMB(baseline.heapUsed)
    );
    console.error('');

    for (const w of workloads) {
        const stats = await benchmark(w, args.runs, args.warmup);
        const mem = snapshotMemory();
        console.error(w.name);
        console.error(
            '  timing   ' +
                'min=' +
                fmtMs(stats.min) +
                '  median=' +
                fmtMs(stats.median) +
                '  mean=' +
                fmtMs(stats.mean) +
                '  max=' +
                fmtMs(stats.max) +
                '   (inputs=' +
                stats.inputs +
                (stats.halted ? ', HALT' : '') +
                ')'
        );
        console.error(
            '  memory   ' +
                'rss=' +
                fmtMB(mem.rss) +
                ' (' +
                fmtDeltaMB(mem.rss - baseline.rss) +
                ')  ' +
                'heapUsed=' +
                fmtMB(mem.heapUsed) +
                ' (' +
                fmtDeltaMB(mem.heapUsed - baseline.heapUsed) +
                ')'
        );
        console.error('');
    }
}

main().catch((e) => {
    console.error(e);
    process.exit(1);
});
