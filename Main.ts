// New optimized TypeScript implementation

async function readInputsFromFile(filePath: string): Promise<[number, string, number]> {
    const text = await Deno.readTextFile(filePath);
    const lines = text.split('\n');
    const ruleNumber = parseInt(lines[0].trim(), 10);
    const initialConditions = lines[1].trim();
    const generations = parseInt(lines[2].trim(), 10);
    return [ruleNumber, initialConditions, generations];
}

function ruleToBinaryArray(ruleNumber: number): Uint8Array {
    const rule = new Uint8Array(8);
    for (let i = 0; i < 8; i++) {
        rule[i] = (ruleNumber >> i) & 1;
    }
    return rule;
}

function runCellularAutomaton(rule: Uint8Array, generations: number, initial: Uint8Array): Uint8Array {
    const width = initial.length + 2 * generations;
    const grid = new Uint8Array(generations * width);
    const curr = new Uint8Array(width);
    const next = new Uint8Array(width);

    // center initial state
    const offset = generations;
    curr.set(initial, offset);

    for (let gen = 0; gen < generations; gen++) {
        // copy current row into grid
        grid.set(curr, gen * width);

        // compute next generation
        for (let j = 1; j < width - 1; j++) {
            const idx = (curr[j - 1] << 2) | (curr[j] << 1) | curr[j + 1];
            next[j] = rule[idx];
        }

        // swap buffers by copying next into curr
        curr.set(next);
    }

    return grid;
}

async function main() {
    const [ruleNumber, initialStr, generations] = await readInputsFromFile('input.txt');
    const initial = new Uint8Array(initialStr.length);
    for (let i = 0; i < initialStr.length; i++) {
        initial[i] = initialStr[i] === '1' ? 1 : 0;
    }

    const rule = ruleToBinaryArray(ruleNumber);
    const grid = runCellularAutomaton(rule, generations, initial);
    const width = initial.length + 2 * generations;

    // build PBM output
    let output = `P1\n${width} ${generations}\n`;
    for (let gen = 0; gen < generations; gen++) {
        const row = grid.subarray(gen * width, gen * width + width);
        for (let j = 0; j < width; j++) {
            output += row[j] === 1 ? '1' : '0';
        }
        output += '\n';
    }

    await Deno.writeTextFile(`results/r${ruleNumber}_g${generations}_i${initialStr}_typescript.pbm`, output);
}

main();
