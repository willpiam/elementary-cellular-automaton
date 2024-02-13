package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func ruleToBinaryArray(ruleNumber int) []int {
	binaryString := fmt.Sprintf("%08b", ruleNumber)
	var array []int
	for _, bit := range binaryString {
		bitInt, _ := strconv.Atoi(string(bit))
		array = append(array, bitInt)
	}
	return array
}

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

func runCellularAutomaton(rule []int, generations int, initialCells []int) [][]int {
	cells := make([]int, len(initialCells))
	copy(cells, initialCells)
	var ca [][]int

	// Directly add the initial state to ca
	ca = append(ca, cells)

	for i := 0; i < generations-1; i++ { // Adjust to generations-1 to include the initial state
		extendedCells := append([]int{0, 0}, cells...)
		extendedCells = append(extendedCells, 0, 0)

		var nextGeneration []int
		for j := 1; j < len(extendedCells)-1; j++ {
			neighborhood := fmt.Sprintf("%d%d%d", extendedCells[j-1], extendedCells[j], extendedCells[j+1])
			nextGeneration = append(nextGeneration, calculateCell(neighborhood, rule))
		}
		cells = nextGeneration
		ca = append(ca, cells)
	}
	return ca
}

func padImageData(imageData [][]int, totalWidth int) [][]int {
	var paddedData [][]int
	for _, row := range imageData {
		paddingLength := (totalWidth - len(row)) / 2
		padding := make([]int, paddingLength)
		paddedRow := append(padding, row...)
		paddedRow = append(paddedRow, padding...)
		paddedData = append(paddedData, paddedRow)
	}
	return paddedData
}

func readInputsFromFile(filePath string) (int, string, int, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return 0, "", 0, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	ruleNumber, _ := strconv.Atoi(lines[0])
	initialConditions := lines[1]
	generations, _ := strconv.Atoi(lines[2])
	return ruleNumber, initialConditions, generations, nil
}

func main() {
	ruleNumber, initialConditions, generations, err := readInputsFromFile("input.txt")
	if err != nil {
		fmt.Println("Error reading input file:", err)
		return
	}

	ruleBinary := ruleToBinaryArray(ruleNumber)
	cells := make([]int, len(initialConditions))
	for i, bit := range initialConditions {
		cells[i], _ = strconv.Atoi(string(bit))
	}

	ca := runCellularAutomaton(ruleBinary, generations, cells)

	finalWidth := len(initialConditions) + 2*generations
	paddedCA := padImageData(ca, finalWidth)

	imageData := fmt.Sprintf("P1\n%d %d\n", finalWidth, generations)
	for _, row := range paddedCA {
		for _, num := range row {
			imageData += strconv.Itoa(num)
		}
		imageData += "\n"
	}

	filePath := fmt.Sprintf("results/r%d_g%d_i%s_go.pbm", ruleNumber, generations, initialConditions)
	file, err := os.Create(filePath)
	if err != nil {
		fmt.Println("Error creating result file:", err)
		return
	}
	defer file.Close()

	_, err = file.WriteString(imageData)
	if err != nil {
		fmt.Println("Error writing to result file:", err)
		return
	}
}
