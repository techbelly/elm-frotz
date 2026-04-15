// Events harness: reads commands from stdin, writes one NDJSON event per
// line to stdout. Use with a ready-built harness/dist/elm.js:
//
//     npm run build-harness
//     node harness/events.js testing/infocom/hhgg-r60-s861028.z3
//
// Each stdout line is a JSON object with an "event" field; see
// harness/Main.elm for the schema. Errors go to stderr; exit code 1 on
// runtime error, 0 on clean halt.

const fs = require('fs');
const readline = require('readline');

const { Elm } = require('./dist/elm.js');

const storyPath = process.argv[2];
if (!storyPath) {
    console.error('Usage: node harness/events.js <story-file>');
    process.exit(2);
}
const storyBytes = fs.readFileSync(storyPath);

const app = Elm.Main.init({ flags: true });

// Line-buffered stdin. When Elm asks for input, we hand it the next line.
// If stdin closes before the game is satisfied we feed empty strings so
// the machine exits cleanly rather than hanging.
const lineQueue = [];
const waiters = [];
let stdinClosed = false;

const rl = readline.createInterface({ input: process.stdin });

rl.on('line', (line) => {
    if (waiters.length > 0) {
        waiters.shift()(line);
    } else {
        lineQueue.push(line);
    }
});

rl.on('close', () => {
    stdinClosed = true;
    while (waiters.length > 0) {
        waiters.shift()('');
    }
});

function nextLine(cb) {
    if (lineQueue.length > 0) {
        cb(lineQueue.shift());
    } else if (stdinClosed) {
        cb('');
    } else {
        waiters.push(cb);
    }
}

app.ports.output.subscribe((text) => {
    // `text` is already NDJSON (zero or more complete lines, each ending in
    // \n). Forward verbatim.
    process.stdout.write(text);
});

app.ports.requestInput.subscribe((signal) => {
    if (signal === 'HALT') {
        rl.close();
        process.exit(0);
        return;
    }
    if (signal === 'CHAR') {
        nextLine((answer) => {
            app.ports.inputProvided.send(answer.slice(0, 1) || '\n');
        });
    } else {
        nextLine((answer) => {
            app.ports.inputProvided.send(answer);
        });
    }
});

app.ports.continueRunning.subscribe(() => {
    setImmediate(() => app.ports.resume.send(null));
});

app.ports.errorOccurred.subscribe((err) => {
    // In events mode the error event was already emitted to stdout; `err`
    // is empty. In text mode it would be a human-readable message.
    if (err) {
        process.stderr.write(err + '\n');
    }
    rl.close();
    process.exit(1);
});

app.ports.storyLoaded.send(Array.from(storyBytes));
