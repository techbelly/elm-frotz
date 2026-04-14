const fs = require('fs');
const { Elm } = require('./dump-elm.js');

const storyPath = process.argv[2];
const commandString = process.argv[3] || '';
if (!storyPath) {
    console.error('Usage: node dump.js <story.z5> [commands-separated-by-|]');
    process.exit(1);
}

const commands = commandString ? commandString.split('|') : [];
const storyBytes = fs.readFileSync(storyPath);
const app = Elm.Dump.init();

app.ports.output.subscribe((text) => {
    console.error('[dump.js] output port received:', text.length, 'chars');
    process.stdout.write(text);
    scheduleExit();
});

setTimeout(() => { console.error('[dump.js] timeout'); process.exit(1); }, 30000);

let timer;
function scheduleExit() {
    if (timer) clearTimeout(timer);
    timer = setTimeout(() => process.exit(0), 100);
}

app.ports.startDump.send({ story: Array.from(storyBytes), commands: commands });
