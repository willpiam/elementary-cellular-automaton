// An attempt at asynchronusly calculating a cellular automata
// such that it can be computed in a less strict order
// in this version of the program the only restriction on where or not a cell can be computed
// is that it's three parents must be computed first
// this means we do not have to compute the entire generation before beginning to compute the next
// though obviously we can never complete generation N before completing generation N-1

type RuleArray = [number, number, number, number, number, number, number, number];

async function readInputsFromFile(filePath: string): Promise<[number, string, number]> {
    const text = await Deno.readTextFile(filePath);
    const lines = text.split('\n');
    const ruleNumber = parseInt(lines[0].trim(), 10);
    const initialConditions = lines[1].trim();
    const generations = parseInt(lines[2].trim(), 10);
    return [ruleNumber, initialConditions, generations];
}

const [ruleNumber, initialConditions, generations] = await readInputsFromFile('input.txt');
console.log(`Rule Number: ${ruleNumber}`);
console.log(`Initial Conditions: ${initialConditions}`);
console.log(`Generations: ${generations}`);

type Cell = {
    state: 'alive' | 'dead' | 'uncomputed'
    position: {
        zeroOffset: number, // negative is left, p
        generation: number, // always positive
    },
    parents: [string, string, string] | null, // these three strings can be used to look up the parent cells in the state map
}

type DefaultZero = "defaultZero"

const state: Map<string, Cell> = new Map()

state.set('0|0', {
    state: 'alive',
    position: {
        zeroOffset: 0,
        generation: 0,
    },
    parents: null,
})

const generation1 = [] as Cell[]


const KeyFromPosition = (p: Cell['position']): string => {
    return `${p.zeroOffset}|${p.generation}`
}

const KeyPointsToMemberOfGenerationZero = (k: string): boolean => {
    const [zeroOffset, generation] = k.split('|')
    return generation === '0'
}


const CalculateParentsFromPosition = (a: Cell): Cell => {
    const { zeroOffset, generation } = a.position

    const parentGeneration = generation - 1
    const p1 = `${a.position.zeroOffset - 1}|${parentGeneration}`
    const p2 = `${a.position.zeroOffset}|${parentGeneration}`
    const p3 = `${a.position.zeroOffset + 1}|${parentGeneration}`

    return {
        ...a,
        parents: [p1, p2, p3],
    }
}

const FindCell = (s: string): Cell | DefaultZero => {
    const result = state.get(s)

    if (result === undefined) {
        // if s is part of generation 0
        if (KeyPointsToMemberOfGenerationZero(s))
            return "defaultZero" as DefaultZero

        throw new Error(`Cell at position ${s} not found in state map`)
        // otherwise we need to compute the cell
    }

    return result as Cell
}

const FindParents = (a: Cell): [Cell | DefaultZero, Cell | DefaultZero, Cell | DefaultZero] => {
    const { parents } = a

    if (parents === null) {
        return FindParents(CalculateParentsFromPosition(a))
    }

    const [p1, p2, p3] = parents

    return [
        FindCell(p1),
        FindCell(p2),
        FindCell(p3),
    ]
}

// const parents = FindParents(t1)

// console.log(parents)
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

function ruleToBinaryArray(ruleNumber: number): RuleArray {
    let binaryString = ruleNumber.toString(2).padStart(8, '0');
    return binaryString.split('').map(bit => parseInt(bit, 10)) as RuleArray;
}

const ParentsToStateString = (p: [Cell | DefaultZero, Cell | DefaultZero, Cell | DefaultZero]): string => {
    const ParentToBit = (p: Cell | DefaultZero): string => {
        if (p === "defaultZero")
            return '0'

        return p.state === 'alive' ? '1' : '0'
    }

    return p.map(ParentToBit).join('')
}

const ComputeCell = (a: Cell): Cell => {
    const parents = FindParents(a)

    const stateString = ParentsToStateString(parents)

    const computedState = calculateCell(stateString, ruleToBinaryArray(ruleNumber)) === 1 ? 'alive' : 'dead'

    const updatedCell: Cell = {
        ...a,
        state: computedState,
    }

    state.set(KeyFromPosition(updatedCell.position), updatedCell)

    return updatedCell
}


// const t1: Cell = {
//     state: 'uncomputed',
//     position: {
//         zeroOffset: -1,
//         generation: 1,
//     },
//     parents: null,
// }

// state.set(KeyFromPosition(t1.position), t1)

const g1 : Cell[]  = Array.from({ length: 3 }, (_, i) => ({
    state: 'uncomputed',
    position: {
        zeroOffset: i - 1,
        generation: 1,
    },
    parents: null,
}) as Cell)

// const computed = ComputeCell(t1)
// console.log(computed)

console.log(`Before computing generation 1`)
console.log(state)

const computed = g1.map(ComputeCell)
console.log(`After computing generation 1`)
console.log(state)