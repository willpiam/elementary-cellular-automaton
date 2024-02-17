package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "time"
)

func ruleToBinaryArray(ruleNumber int) []int {
    binary := fmt.Sprintf("%08b", ruleNumber)
    ruleBinary := make([]int, 8)

    for i, ch := range binary {
        if ch == '1' {
            ruleBinary[7-i] = 1
        } else {
            ruleBinary[7-i] = 0
        }
    }

    return ruleBinary
}

func calculateCell(neighborhood string, ruleBinary []int) int {
    index, _ := strconv.ParseInt(neighborhood, 2, 64)
    return ruleBinary[index]
}

func runCellularAutomaton(ruleNumber, generations int, initialConditions string) [][]int {
    cells := make([]int, len(initialConditions))
    for i, bit := range initialConditions {
        if bit == '1' {
            cells[i] = 1
        } else {
            cells[i] = 0
        }
    }

    ruleBinary := ruleToBinaryArray(ruleNumber)
    imageWidth := len(cells) + 2*generations
    var automatonData [][]int

    for i := 0; i < generations; i++ {
        paddingLength := (imageWidth - len(cells)) / 2
        padding := make([]int, paddingLength)
        extendedCells := append(append(padding, cells...), padding...)

        automatonData = append(automatonData, extendedCells)

        nextGeneration := make([]int, len(extendedCells))
        for j := range extendedCells {
            leftNeighbor := 0
            if j > 0 {
                leftNeighbor = extendedCells[j-1]
            }
            rightNeighbor := 0
            if j < len(extendedCells)-1 {
                rightNeighbor = extendedCells[j+1]
            }

            neighborhood := fmt.Sprintf("%d%d%d", leftNeighbor, extendedCells[j], rightNeighbor)
            nextGeneration[j] = calculateCell(neighborhood, ruleBinary)
        }
        cells = nextGeneration
    }

    return automatonData
}

func outputToFile(automatonData [][]int, ruleNumber, generations int, initialConditions string) {
    filename := fmt.Sprintf("results/r%d_g%d_i%s_go.pbm", ruleNumber, generations, initialConditions)
    file, err := os.Create(filename)
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := bufio.NewWriter(file)
    imageWidth := 0
    if len(automatonData) > 0 {
        imageWidth = len(automatonData[0])
    }

    fmt.Fprintf(writer, "P1\n%d %d\n", imageWidth, generations)
    for _, row := range automatonData {
        for _, cell := range row {
            if cell == 1 {
                writer.WriteString("1")
            } else {
                writer.WriteString("0")
            }
        }
        writer.WriteString("\n")
    }
    writer.Flush()
}

func main() {
    inputFile, err := os.Open("input.txt")
    if err != nil {
        fmt.Println("Error opening input file!")
        os.Exit(1)
    }
    defer inputFile.Close()

    scanner := bufio.NewScanner(inputFile)
    scanner.Scan()
    ruleNumber, _ := strconv.Atoi(scanner.Text())
    scanner.Scan()
    initialConditions := scanner.Text()
    scanner.Scan()
    generations, _ := strconv.Atoi(scanner.Text())

    start := time.Now()

    automatonData := runCellularAutomaton(ruleNumber, generations, initialConditions)
    outputToFile(automatonData, ruleNumber, generations, initialConditions)

    duration := time.Since(start)
    fmt.Printf("Took %v to generate %d generations of rule %d\n", duration, generations, ruleNumber)
    fmt.Println("Done!")
}
