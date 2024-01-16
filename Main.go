// package main

// import "fmt"

// func main() {
//     fmt.Println("Hello, World!")
// }

package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
    "time"
)

// Convert rule number to binary array
func ruleToBinaryArray(ruleNumber int) []int {
    binaryString := fmt.Sprintf("%08b", ruleNumber)
    binaryArray := make([]int, 8)
    for i, bit := range binaryString {
        binaryArray[i], _ = strconv.Atoi(string(bit))
    }
    return binaryArray
}

// Calculate the next state of a cell based on the rule
func calculateCell(pState string, rule []int) int {
    ruleMap := map[string]int{
        "111": rule[0],
        "110": rule[1],
        "101": rule[2],
        "100": rule[3],
        "011": rule[4],
        "010": rule[5],
        "001": rule[6],
        "000": rule[7],
    }
    return ruleMap[pState]
}

// Run cellular automaton based on the given rule number, generations, and initial conditions
func runCellularAutomaton(ruleNumber int, generations int, initialConditions string) {
    cells := make([]int, len(initialConditions))
    for i, bit := range initialConditions {
        cells[i], _ = strconv.Atoi(string(bit))
    }

    ruleBinary := ruleToBinaryArray(ruleNumber)

    imageWidth := len(cells) + 2*generations
    imageData := fmt.Sprintf("P1\n%d %d\n", imageWidth, generations)

    startTime := time.Now()

    for i := 0; i < generations; i++ {
        paddingLength := (imageWidth - len(cells)) / 2
        padding := strings.Repeat("0", paddingLength)
        extendedCells := padding + strings.Trim(strings.Replace(fmt.Sprint(cells), " ", "", -1), "[]") + padding

        imageData += extendedCells + "\n"

        nextGeneration := make([]int, len(cells))
        for j := 1; j < len(cells)+1; j++ {
            neighborhood := extendedCells[j-1 : j+2]
            nextGeneration[j-1] = calculateCell(neighborhood, ruleBinary)
        }
        cells = nextGeneration
    }

    endTime := time.Now()
    fmt.Printf("Took %v to generate %d generations of rule %d\n", endTime.Sub(startTime), generations, ruleNumber)

    file, err := os.Create(fmt.Sprintf("results/r%d_g%d_i%s_go.pbm", ruleNumber, generations, initialConditions))
    if err != nil {
        fmt.Println("Error creating file:", err)
        return
    }
    defer file.Close()

    file.WriteString(imageData)
}

// Read inputs from file
func readInputsFromFile(filePath string) (int, string, int, error) {
    file, err := os.Open(filePath)
    if err != nil {
        return 0, "", 0, err
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    scanner.Scan()
    ruleNumber, _ := strconv.Atoi(scanner.Text())
    scanner.Scan()
    initialConditions := scanner.Text()
    scanner.Scan()
    generations, _ := strconv.Atoi(scanner.Text())

    return ruleNumber, initialConditions, generations, nil
}

// Main function to run the program
func main() {
    ruleNumber, initialConditions, generations, err := readInputsFromFile("input.txt")
    if err != nil {
        fmt.Println("Error reading input file:", err)
        return
    }

    fmt.Printf("Rule Number: %d\n", ruleNumber)
    fmt.Printf("Initial Conditions: %s\n", initialConditions)
    fmt.Printf("Generations: %d\n", generations)

    runCellularAutomaton(ruleNumber, generations, initialConditions)
}
