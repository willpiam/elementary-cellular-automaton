function die(msg: string): never {
    console.error(msg);
    Deno.exit(1);
}

function parseIntStrict(text: string): number | null {
    const t = text.trim();
    const neg = t.startsWith('-');
    const digits = neg ? t.slice(1) : t;
    if (!/^\d+$/.test(digits)) return null;
    const value = Number(digits);
    if (!Number.isSafeInteger(value)) return null;
    return neg ? -value : value;
}

function validSeed(seed: string): boolean {
    return /^[01]+$/.test(seed);
}

let text: string;
try {
    text = await Deno.readTextFile('input.txt');
} catch (_err) {
    die('cannot read input.txt');
}
const lines = text.split(/\r?\n/);
if (lines.length < 3) die('invalid input');

const rule = parseIntStrict(lines[0]);
if (rule === null || rule < 0 || rule > 255) die('invalid rule');
const seed = lines[1];
if (!validSeed(seed)) die('invalid initial conditions');
const generations = parseIntStrict(lines[2]);
if (generations === null || generations < 1) die('invalid generations');

const n = seed.length;
const width = n + 2 * generations;
if (!Number.isSafeInteger(width)) die('invalid generations');
const table = new Uint8Array(8);
for (let i = 0; i < 8; i++) table[i] = (rule >> i) & 1;
const even = (rule & 1) === 0;
let prev = new Uint8Array(width);
let next = new Uint8Array(width);
for (let i = 0; i < n; i++) {
    if (seed[i] === '1') prev[generations + i] = 1;
}

await Deno.mkdir('results', { recursive: true });
const path = `results/r${rule}_g${generations}_i${seed}_typescript.pbm`;
const file = await Deno.create(path);
const encoder = new TextEncoder();
await file.write(encoder.encode(`P1\n${width} ${generations}\n`));
const line = new Uint8Array(width + 1);
line[width] = 10;
for (let gen = 0; gen < generations; gen++) {
    for (let j = 0; j < width; j++) line[j] = prev[j] === 0 ? 48 : 49;
    await file.write(line);
    if (gen + 1 === generations) break;
    next.fill(0);
    const lo = even ? generations - (gen + 1) : 0;
    const hi = even ? generations + n + (gen + 1) : width;
    for (let j = lo; j < hi; j++) {
        const left = j > 0 ? prev[j - 1] : 0;
        const right = j + 1 < width ? prev[j + 1] : 0;
        const idx = (left << 2) | (prev[j] << 1) | right;
        next[j] = table[idx];
    }
    const tmp = prev;
    prev = next;
    next = tmp;
}
file.close();
