const fs = require('fs').promises;

function ruleToBinaryArray(ruleNumber) {
    return ruleNumber.toString(2).padStart(8, '0').split('').map(bit => parseInt(bit, 10));
}

function calculateCell(pState, rule) {
    const ruleMap = {
        "111": rule[0],
        "110": rule[1],
        "101": rule[2],
        "100": rule[3],
        "011": rule[4],
        "010": rule[5],
        "001": rule[6],
        "000": rule[7]
    };
    return ruleMap[pState];
}

async function runCellularAutomaton(rule, generations, initialCells) {
    let cells = JSON.parse(JSON.stringify(initialCells));

    let ca = [];
    const padding = Array(2).fill(0);

    for (let i = 0; i < generations - 1; i++) {
        const extendedCells = [...padding, ...cells, ...padding];
        ca.push(extendedCells);

        const nextGeneration = [];

        for (let j = 1; j < extendedCells.length - 1; j++) {
            const neighborhood = '' + extendedCells[j - 1] + extendedCells[j] + extendedCells[j + 1];
            nextGeneration.push(calculateCell(neighborhood, rule));
        }
        cells = nextGeneration;
    }

    ca.push(cells);
    return ca;
}

async function readInputsFromFile(filePath) {
    const text = await fs.readFile(filePath, 'utf8');
    const lines = text.split('\n');
    const ruleNumber = parseInt(lines[0].trim(), 10);
    const initialConditions = lines[1].trim();
    const generations = parseInt(lines[2].trim(), 10);
    return [ruleNumber, initialConditions, generations];
}

const padCellularAutomaton = (ca, totalWidth) => ca.map(row => {
    const paddingLength = Math.floor((totalWidth - row.length) / 2);
    const padding = Array(paddingLength).fill(0);
    return [...padding, ...row, ...padding];
});

// Main function to run the program
async function main() {
    const [ruleNumber, initialConditions, generations] = await readInputsFromFile('input.txt');

    const initialCells = initialConditions.split('').map(bit => parseInt(bit, 10));
    const rule = ruleToBinaryArray(ruleNumber);

    const ca = await runCellularAutomaton(rule, generations, initialCells);

    const finalWidth = initialConditions.length + 2 * generations;
    const paddedCA = padCellularAutomaton(ca, finalWidth);
    const ca_body = paddedCA.map(row => row.join('')).join('\n');
    const imageData = `P1\n${finalWidth} ${generations}\n${ca_body}\n`;
    
    await fs.writeFile(`results/r${ruleNumber}_g${generations}_i${initialConditions}_javascript.pbm`, imageData);
}

main();
