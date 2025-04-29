import scala.io.Source
import java.io.PrintWriter

object CellularAutomaton: 

  def ruleToBinaryArray(ruleNumber: Int): Array[Int] =
    Array.tabulate(8)(i => (ruleNumber >> i) & 1)

  def runCellularAutomaton(ruleNumber: Int, generations: Int, initialConditions: String): Unit =
    val initArr = initialConditions.view.map(_.asDigit).toArray
    val imageWidth = initArr.length + 2 * generations
    val ruleBinary = ruleToBinaryArray(ruleNumber)

    new java.io.File("results").mkdirs()
    val writer = new PrintWriter(s"results/r${ruleNumber}_g${generations}_i${initialConditions}_scala.pbm")
    writer.println(s"P1\n$imageWidth $generations")

    val current = Array.fill(imageWidth)(0)
    val next    = Array.ofDim[Int](imageWidth)
    Array.copy(initArr, 0, current, generations, initArr.length)
    val rowChars = new Array[Char](imageWidth + 1)

    for (_ <- 0 until generations) do
      var j = 0
      while j < imageWidth do
        rowChars(j) = if current(j) == 1 then '1' else '0'
        j += 1
      rowChars(imageWidth) = '\n'
      writer.write(rowChars)

      var i = 0
      while i < imageWidth do
        val l = if i > 0                 then current(i-1) else 0
        val c =                             current(i)
        val r = if i < imageWidth - 1 then current(i+1) else 0
        next(i) = ruleBinary((l << 2) | (c << 1) | r)
        i += 1
      System.arraycopy(next, 0, current, 0, imageWidth)

    writer.close()

  def readInputsFromFile(filePath: String): (Int, String, Int) = 
    val lines = Source.fromFile(filePath).getLines().toList
    val ruleNumber = lines(0).toInt
    val initialConditions = lines(1)
    val generations = lines(2).toInt
    (ruleNumber, initialConditions, generations)

  def main(args: Array[String]): Unit = 
    val (ruleNumber, initialConditions, generations) = readInputsFromFile("input.txt")
    runCellularAutomaton(ruleNumber, generations, initialConditions)

// This is the correct way to invoke the main method in Scala.
object Main extends App: 
  CellularAutomaton.main(Array())
