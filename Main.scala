import java.io.{BufferedOutputStream, File, FileOutputStream}
import scala.io.Source
import scala.util.Using
import java.util.Arrays

object CellularAutomaton {
  def die(msg: String): Nothing = {
    System.err.println(msg)
    sys.exit(1)
  }

  def parseInt(text: String): Option[Int] = {
    val t = text.trim.stripSuffix("\r").trim
    val (neg, digits) =
      if (t.startsWith("-")) (true, t.drop(1))
      else (false, t)
    if (digits.isEmpty || !digits.forall(_.isDigit)) None
    else {
      try {
        val value = BigInt(digits)
        if (value > Int.MaxValue) None
        else Some(if (neg) (-value).toInt else value.toInt)
      } catch {
        case _: NumberFormatException => None
      }
    }
  }

  def validSeed(seed: String): Boolean =
    seed.nonEmpty && seed.forall(c => c == '0' || c == '1')

  def main(args: Array[String]): Unit = {
    val lines = Using(Source.fromFile("input.txt"))(_.getLines().toList).getOrElse(die("cannot read input.txt"))
    if (lines.length < 3) die("invalid input")
    val rule = parseInt(lines(0)).filter(v => v >= 0 && v <= 255).getOrElse(die("invalid rule"))
    val seed = lines(1).stripSuffix("\r")
    if (!validSeed(seed)) die("invalid initial conditions")
    val generations = parseInt(lines(2)).filter(_ >= 1).getOrElse(die("invalid generations"))
    val n = seed.length
    if (generations > (Int.MaxValue - n) / 2) die("invalid generations")
    val width = n + 2 * generations
    val even = (rule & 1) == 0
    val table = Array.tabulate(8)(i => ((rule >> i) & 1).toByte)
    var prev = new Array[Byte](width)
    var next = new Array[Byte](width)
    var i = 0
    while (i < n) {
      if (seed.charAt(i) == '1') prev(generations + i) = 1
      i += 1
    }

    new File("results").mkdirs()
    val path = s"results/r${rule}_g${generations}_i${seed}_scala.pbm"
    val out = new BufferedOutputStream(new FileOutputStream(path))
    try {
      out.write(s"P1\n$width $generations\n".getBytes("US-ASCII"))
      val line = new Array[Byte](width + 1)
      line(width) = '\n'.toByte
      var gen = 0
      while (gen < generations) {
        var j = 0
        while (j < width) {
          line(j) = if (prev(j) == 0) '0'.toByte else '1'.toByte
          j += 1
        }
        out.write(line)
        if (gen + 1 != generations) {
          Arrays.fill(next, 0.toByte)
          val lo = if (even) generations - (gen + 1) else 0
          val hi = if (even) generations + n + (gen + 1) else width
          j = lo
          while (j < hi) {
            val left = if (j > 0) prev(j - 1).toInt else 0
            val right = if (j + 1 < width) prev(j + 1).toInt else 0
            val idx = (left << 2) | (prev(j).toInt << 1) | right
            next(j) = table(idx)
            j += 1
          }
          val tmp = prev
          prev = next
          next = tmp
        }
        gen += 1
      }
    } finally {
      out.close()
    }
  }
}
