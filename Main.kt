import java.io.File

fun ruleToBinaryArray(ruleNumber: Int): List<Int> {
    return ruleNumber.toString(2).padStart(8, '0').map { it.toString().toInt() }
}

fun calculateCell(pState: String, rule: List<Int>): Int {
    val ruleMap = mapOf(
        "111" to rule[0],
        "110" to rule[1],
        "101" to rule[2],
        "100" to rule[3],
        "011" to rule[4],
        "010" to rule[5],
        "001" to rule[6],
        "000" to rule[7]
    )
    return ruleMap[pState]!!
}

fun runCellularAutomaton(rule: List<Int>, generations: Int, initialCells: List<Int>): List<List<Int>> {
    var cells = initialCells.toList()
    val ca = mutableListOf<List<Int>>()

    repeat(generations - 1) {
        val extendedCells = listOf(0, 0) + cells + listOf(0, 0)
        ca.add(cells)

        val nextGeneration = mutableListOf<Int>()
        for (j in 1 until extendedCells.size - 1) {
            val neighborhood = (j-1..j+1).joinToString("") { extendedCells[it].toString() }
            nextGeneration.add(calculateCell(neighborhood, rule))
        }
        cells = nextGeneration
    }

    ca.add(cells)
    return ca
}

fun padImageData(imageData: List<List<Int>>, totalWidth: Int): List<List<Int>> {
    return imageData.map { row ->
        val paddingLength = (totalWidth - row.size) / 2
        val padding = List(paddingLength) { 0 }
        padding + row + padding
    }
}

fun readInputsFromFile(filePath: String): Triple<Int, String, Int> {
    val lines = File(filePath).readLines()
    val ruleNumber = lines[0].toInt()
    val initialConditions = lines[1]
    val generations = lines[2].toInt()
    return Triple(ruleNumber, initialConditions, generations)
}

fun main() {
    val (ruleNumber, initialConditions, generations) = readInputsFromFile("input.txt")

    val ruleBinary = ruleToBinaryArray(ruleNumber)
    val cells = initialConditions.map { it.toString().toInt() }

    val ca = runCellularAutomaton(ruleBinary, generations, cells)

    val finalWidth = initialConditions.length + 2 * generations
    val paddedCa = padImageData(ca, finalWidth)

    val imageBuilder = StringBuilder()
    imageBuilder.append("P1\n$finalWidth $generations\n")
    paddedCa.forEach { row ->
        imageBuilder.append(row.joinToString("") { it.toString() }).append("\n")
    }

    File("results/r${ruleNumber}_g${generations}_i${initialConditions}_kotlin.pbm").writeText(imageBuilder.toString())
}
