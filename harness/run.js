const fs = require('fs');
const readline = require('readline');

// Load compiled Elm
const { Elm } = require('./elm.js');

// Read the story file
const storyPath = process.argv[2] || 'testing/Zork1.z3';
const storyBytes = fs.readFileSync(storyPath);

// Create a DataView from the buffer for Elm's Bytes format
const app = Elm.Main.init();

// Set up readline for interactive input
const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: ''
});

app.ports.output.subscribe((text) => {
    process.stdout.write(text);
});

app.ports.requestInput.subscribe((signal) => {
    if (signal === 'HALT') {
        rl.close();
        process.exit(0);
        return;
    }
    rl.question('> ', (answer) => {
        app.ports.inputProvided.send(answer);
    });
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
