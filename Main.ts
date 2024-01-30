type RuleArray = [number, number, number, number, number, number, number, number];

function ruleToBinaryArray(ruleNumber: number): RuleArray {
    return ruleNumber.toString(2).padStart(8, '0').split('').map(bit => parseInt(bit, 10)) as RuleArray;
}

function calculateCell(pState: string, rule: RuleArray): number {
    const ruleMap: Record<string, number> = {
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

const startTimer = () => {
    const start = performance.now();
    return () => performance.now() - start;
}

async function runCellularAutomaton(rule: RuleArray, generations: number, initialCells: number[]): Promise<number[][]> {
    let cells = JSON.parse(JSON.stringify(initialCells));

    // Calculate image width: initial conditions length + 2 cells for each generation
    const imageWidth = cells.length + 2 * generations;
    let imageData: number[][] = [];

    for (let i = 0; i < generations; i++) {
        // Calculate padding to center the cells
        const paddingLength = Math.floor((imageWidth - cells.length) / 2);
        const padding = Array(paddingLength).fill(0);
        const extendedCells = [...padding, ...cells, ...padding];

        imageData.push(extendedCells.map(cell => cell ? 1 : 0));

        const nextGeneration: number[] = [];
        for (let j = 1; j < extendedCells.length - 1; j++) {
            const leftNeighbor = extendedCells[j - 1];
            const currentCell = extendedCells[j];
            const rightNeighbor = extendedCells[j + 1];
            const neighborhood = '' + leftNeighbor + currentCell + rightNeighbor;
            nextGeneration[j - 1] = calculateCell(neighborhood, rule);
        }
        cells = nextGeneration;
    }

    return imageData;
}

async function readInputsFromFile(filePath: string): Promise<[number, string, number]> {
    const text = await Deno.readTextFile(filePath);
    const lines = text.split('\n');
    const ruleNumber = parseInt(lines[0].trim(), 10);
    const initialConditions = lines[1].trim();
    const generations = parseInt(lines[2].trim(), 10);
    return [ruleNumber, initialConditions, generations];
}

const padCellularAutomaton = (ca: number[][], totalWidth: number): number[][] => ca.map(row => {
    const paddingLength = Math.floor((totalWidth - row.length) / 2);
    const padding = Array(paddingLength).fill(0);
    return [...padding, ...row, ...padding];
});

// Main function to run the program
async function main() {
    const [ruleNumber, initialConditions, generations] = await readInputsFromFile('input.txt');
    console.log(`Rule Number: ${ruleNumber}`);
    console.log(`Initial Conditions: ${initialConditions}`);
    console.log(`Generations: ${generations}`);

    const timer = startTimer();

    const finalWidth = initialConditions.length + 2 * generations;
    const initialCells = initialConditions.split('').map(bit => parseInt(bit, 10));

    const rule = ruleToBinaryArray(ruleNumber);
    const ca: number[][] = await runCellularAutomaton(rule, generations, initialCells);
    const paddedCA = padCellularAutomaton(ca, finalWidth);
    const ca_body = paddedCA.map(row => row.join('')).join('\n');
    const imageData = `P1\n${finalWidth} ${generations}\n${ca_body}\n`;

    console.log(`Took ${timer().toFixed(2)}ms to generate ${generations} generations of rule ${ruleNumber}`);

    await Deno.writeTextFile(`results/r${ruleNumber}_g${generations}_i${initialConditions}_typescript.pbm`, imageData);
}

main();
