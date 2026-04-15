const fs = require('fs');
const readline = require('readline');

// Load compiled Elm
const { Elm } = require('./dist/elm.js');

// Read the story file
const storyPath = process.argv[2] || 'testing/Zork1.z3';
const storyBytes = fs.readFileSync(storyPath);

// Create a DataView from the buffer for Elm's Bytes format
const app = Elm.Main.init({ flags: false });

// Manual line-buffered reader so we don't crash when stdin closes
// before the game has finished asking for input.
const lineQueue = [];
const waiters = [];
let stdinClosed = false;

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: ''
});

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

function nextLine(prompt, cb) {
    if (lineQueue.length > 0) {
        process.stdout.write(prompt);
        process.stdout.write(lineQueue[0] + '\n');
        cb(lineQueue.shift());
    } else if (stdinClosed) {
        cb('');
    } else {
        process.stdout.write(prompt);
        waiters.push(cb);
    }
}

app.ports.output.subscribe((text) => {
    process.stdout.write(text);
});

app.ports.requestInput.subscribe((signal) => {
    if (signal === 'HALT') {
        rl.close();
        process.exit(0);
        return;
    }
    if (signal === 'CHAR') {
        nextLine('[key] ', (answer) => {
            app.ports.inputProvided.send(answer.slice(0, 1) || '\n');
        });
    } else {
        nextLine('> ', (answer) => {
            app.ports.inputProvided.send(answer);
        });
    }
});

app.ports.continueRunning.subscribe(() => {
    setImmediate(() => app.ports.resume.send(null));
});

app.ports.errorOccurred.subscribe((err) => {
    console.error('\n' + err);
    rl.close();
    process.exit(1);
});

// Send the story file bytes to Elm as a list of ints
app.ports.storyLoaded.send(Array.from(storyBytes));
