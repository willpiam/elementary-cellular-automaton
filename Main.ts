type RuleArray = [number, number, number, number, number, number, number, number];

function ruleToBinaryArray(ruleNumber: number): RuleArray {
    let binaryString = ruleNumber.toString(2).padStart(8, '0');
    return binaryString.split('').map(bit => parseInt(bit, 10)) as RuleArray;
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

async function runCellularAutomaton(ruleNumber: number, generations: number, initialConditions: string): Promise<void> {
    let cells = initialConditions.split('').map(bit => parseInt(bit, 10));

    let ruleBinary = ruleToBinaryArray(ruleNumber);

    // Calculate image width: initial conditions length + 2 cells for each generation
    let imageWidth = cells.length + 2 * generations;
    let imageData = `P1\n${imageWidth} ${generations}\n`;

    const timer = startTimer();

    for (let i = 0; i < generations; i++) {
        // Calculate padding to center the cells
        let paddingLength = Math.floor((imageWidth - cells.length) / 2);
        let padding = Array(paddingLength).fill(0);
        let extendedCells = [...padding, ...cells, ...padding];

        imageData += extendedCells.map(cell => cell ? '1' : '0').join('') + '\n';

        let nextGeneration: number[] = [];
        for (let j = 1; j < extendedCells.length - 1; j++) {
            let leftNeighbor = extendedCells[j - 1];
            let currentCell = extendedCells[j];
            let rightNeighbor = extendedCells[j + 1];
            let neighborhood = '' + leftNeighbor + currentCell + rightNeighbor;
            nextGeneration[j - 1] = calculateCell(neighborhood, ruleBinary);
        }
        cells = nextGeneration;
    }

    console.log(`Took ${timer().toFixed(2)}ms to generate ${generations} generations of rule ${ruleNumber}`);

    await Deno.writeTextFile(`results/r${ruleNumber}_g${generations}_i${initialConditions}_typescript.pbm`, imageData);
}


async function readInputsFromFile(filePath: string): Promise<[number, string, number]> {
    const text = await Deno.readTextFile(filePath);
    const lines = text.split('\n');
    const ruleNumber = parseInt(lines[0].trim(), 10);
    const initialConditions = lines[1].trim();
    const generations = parseInt(lines[2].trim(), 10);
    return [ruleNumber, initialConditions, generations];
}

// Main function to run the program
async function main() {
    const [ruleNumber, initialConditions, generations] = await readInputsFromFile('input.txt');
    console.log(`Rule Number: ${ruleNumber}`);
    console.log(`Initial Conditions: ${initialConditions}`);
    console.log(`Generations: ${generations}`);

    runCellularAutomaton(ruleNumber, generations, initialConditions);
}

main();
