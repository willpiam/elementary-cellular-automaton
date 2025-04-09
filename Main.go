package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
)

func ruleToBinaryArray(ruleNumber int) []int {
    ruleBinary := make([]int, 8)
    for i := 0; i < 8; i++ {
        ruleBinary[i] = (ruleNumber >> i) & 1
    }
    return ruleBinary
}

func calculateCell(neighborhood int, ruleBinary []int) int {
    return ruleBinary[neighborhood]
}

func runCellularAutomaton(ruleNumber, generations int, initialConditions string) [][]int {
    cells := make([]int, len(initialConditions))
    for i, bit := range initialConditions {
        cells[i] = int(bit - '0')
    }

    ruleBinary := ruleToBinaryArray(ruleNumber)
    imageWidth := len(cells) + 2*generations
    automatonData := make([][]int, generations)
    nextGeneration := make([]int, imageWidth)

    for i := 0; i < generations; i++ {
        paddingLength := (imageWidth - len(cells)) / 2
        extendedCells := make([]int, imageWidth)
        copy(extendedCells[paddingLength:], cells)

        automatonData[i] = make([]int, imageWidth)
        copy(automatonData[i], extendedCells)

        for j := 0; j < imageWidth; j++ {
            leftNeighbor := 0
            if j > 0 {
                leftNeighbor = extendedCells[j-1]
            }
            rightNeighbor := 0
            if j < imageWidth-1 {
                rightNeighbor = extendedCells[j+1]
            }

            neighborhood := (leftNeighbor << 2) | (extendedCells[j] << 1) | rightNeighbor
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

    // Pre-allocate buffer for the header
    header := fmt.Sprintf("P1\n%d %d\n", imageWidth, generations)
    writer.WriteString(header)

    // Pre-allocate buffer for each row
    rowBuffer := make([]byte, imageWidth+1) // +1 for newline
    for _, row := range automatonData {
        for j, cell := range row {
            if cell == 1 {
                rowBuffer[j] = '1'
            } else {
                rowBuffer[j] = '0'
            }
        }
        rowBuffer[imageWidth] = '\n'
        writer.Write(rowBuffer)
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

    automatonData := runCellularAutomaton(ruleNumber, generations, initialConditions)
    outputToFile(automatonData, ruleNumber, generations, initialConditions)
}
