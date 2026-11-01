import java.io.File
import java.io.FileOutputStream
import kotlin.system.exitProcess

fun die(msg: String): Nothing {
    System.err.println(msg)
    exitProcess(1)
}

fun parseIntStrict(text: String): Int? {
    val t = text.trim().trimEnd('\r')
    if (t.isEmpty()) return null
    val neg = t[0] == '-'
    val digits = if (neg) t.substring(1) else t
    if (digits.isEmpty() || digits.any { it !in '0'..'9' }) return null
    val value = digits.toLongOrNull() ?: return null
    if (value > Int.MAX_VALUE) return null
    return if (neg) (-value).toInt() else value.toInt()
}

fun validSeed(seed: String): Boolean = seed.isNotEmpty() && seed.all { it == '0' || it == '1' }

fun main() {
    val lines = try {
        File("input.txt").readLines()
    } catch (_: Exception) {
        die("cannot read input.txt")
    }
    if (lines.size < 3) die("invalid input")

    val rule = parseIntStrict(lines[0]) ?: die("invalid rule")
    if (rule !in 0..255) die("invalid rule")
    val seed = lines[1].trimEnd('\r')
    if (!validSeed(seed)) die("invalid initial conditions")
    val generations = parseIntStrict(lines[2]) ?: die("invalid generations")
    if (generations < 1) die("invalid generations")

    val n = seed.length
    if (generations > (Int.MAX_VALUE - n) / 2) die("invalid generations")
    val width = n + 2 * generations
    val even = rule and 1 == 0
    val table = ByteArray(8) { i -> ((rule shr i) and 1).toByte() }
    var prev = ByteArray(width)
    var next = ByteArray(width)
    seed.forEachIndexed { i, ch ->
        if (ch == '1') prev[generations + i] = 1
    }

    File("results").mkdirs()
    val path = "results/r${rule}_g${generations}_i${seed}_kotlin.pbm"
    FileOutputStream(path).use { out ->
        out.write("P1\n$width $generations\n".toByteArray(Charsets.US_ASCII))
        val line = ByteArray(width + 1)
        line[width] = '\n'.code.toByte()
        for (gen in 0 until generations) {
            for (j in 0 until width) {
                line[j] = if (prev[j].toInt() == 0) '0'.code.toByte() else '1'.code.toByte()
            }
            out.write(line)
            if (gen + 1 == generations) break
            next.fill(0)
            val lo = if (even) generations - (gen + 1) else 0
            val hi = if (even) generations + n + (gen + 1) else width
            for (j in lo until hi) {
                val left = if (j > 0) prev[j - 1].toInt() else 0
                val right = if (j + 1 < width) prev[j + 1].toInt() else 0
                val idx = (left shl 2) or (prev[j].toInt() shl 1) or right
                next[j] = table[idx]
            }
            val tmp = prev
            prev = next
            next = tmp
        }
    }
}
