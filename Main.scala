import scala.io.Source
import java.io.PrintWriter
import scala.collection.mutable.ListBuffer

object CellularAutomaton {

  def ruleToBinaryArray(ruleNumber: Int): Array[Int] = {
    val binaryString = ruleNumber.toBinaryString.reverse.padTo(8, '0').reverse
    binaryString.map(_.asDigit).toArray
  }

  def calculateCell(pState: String, rule: Array[Int]): Int = {
    val ruleMap = Map(
      "111" -> rule(0),
      "110" -> rule(1),
      "101" -> rule(2),
      "100" -> rule(3),
      "011" -> rule(4),
      "010" -> rule(5),
      "001" -> rule(6),
      "000" -> rule(7)
    )
    ruleMap(pState)
  }

  def runCellularAutomaton(ruleNumber: Int, generations: Int, initialConditions: String): Unit = {
    var cells = initialConditions.map(_.asDigit).toArray

    val ruleBinary = ruleToBinaryArray(ruleNumber)

    val imageWidth = cells.length + 2 * generations
    val imageData = new StringBuilder(s"P1\n$imageWidth $generations\n")

    val startTime = System.nanoTime()

    for (_ <- 0 until generations) {
      val paddingLength = (imageWidth - cells.length) / 2
      val padding = Array.fill(paddingLength)(0)
      val extendedCells = padding ++ cells ++ padding

      imageData.append(extendedCells.map(cell => if (cell == 0) '0' else '1').mkString + "\n")

      val nextGeneration = new ListBuffer[Int]()
      for (j <- 1 until extendedCells.length - 1) {
        val neighborhood = s"${extendedCells(j - 1)}${extendedCells(j)}${extendedCells(j + 1)}"
        nextGeneration += calculateCell(neighborhood, ruleBinary)
      }
      cells = nextGeneration.toArray
    }

    val endTime = System.nanoTime()
    println(f"Took ${(endTime - startTime) / 1e6}%.2fms to generate $generations generations of rule $ruleNumber")

    new PrintWriter(s"results/r${ruleNumber}_g${generations}_i${initialConditions}_scala.pbm") {
      write(imageData.toString); close()
    }
  }

  def readInputsFromFile(filePath: String): (Int, String, Int) = {
    val lines = Source.fromFile(filePath).getLines().toList
    val ruleNumber = lines(0).toInt
    val initialConditions = lines(1)
    val generations = lines(2).toInt
    (ruleNumber, initialConditions, generations)
  }

  def main(args: Array[String]): Unit = {
    val (ruleNumber, initialConditions, generations) = readInputsFromFile("input.txt")
    println(s"Rule Number: $ruleNumber")
    println(s"Initial Conditions: $initialConditions")
    println(s"Generations: $generations")

    runCellularAutomaton(ruleNumber, generations, initialConditions)
  }
}

// This is the correct way to invoke the main method in Scala.
object Main extends App {
  CellularAutomaton.main(Array())
}
