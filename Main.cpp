#include <iostream>
#include <fstream>
#include <vector>
#include <bitset>
#include <string>
#include <chrono>

std::vector<int> ruleToBinaryArray(const int ruleNumber) {
    std::bitset<8> binary(ruleNumber);
    std::vector<int> ruleBinary(8);

    for (int i = 0; i < 8; i++) {
        ruleBinary[i] = binary[i];
    }

    return ruleBinary;
}

int calculateCell(const std::string &neighborhood, const std::vector<int> &ruleBinary) {
    int index = std::stoi(neighborhood, nullptr, 2);
    return ruleBinary[index];
}

std::vector<std::vector<int>> runCellularAutomaton(const int ruleNumber, const int generations, const std::string &initialConditions, const std::chrono::high_resolution_clock::time_point start) {
    std::vector<int> cells;
    for (char bit : initialConditions) {
        cells.push_back(bit == '1' ? 1 : 0);
    }

    std::vector<int> ruleBinary = ruleToBinaryArray(ruleNumber);
    int imageWidth = cells.size() + 2 * generations;
    std::vector<std::vector<int>> automatonData;

    for (int i = 0; i < generations; i++) {
        int paddingLength = (imageWidth - cells.size()) / 2;
        std::vector<int> padding(paddingLength, 0);
        std::vector<int> extendedCells(padding);
        extendedCells.insert(extendedCells.end(), cells.begin(), cells.end());
        extendedCells.insert(extendedCells.end(), padding.begin(), padding.end());

        automatonData.push_back(extendedCells);

        std::vector<int> nextGeneration(extendedCells.size());
        for (size_t j = 0; j < extendedCells.size(); j++) {
            int leftNeighbor = j > 0 ? extendedCells[j - 1] : 0;
            int currentCell = extendedCells[j];
            int rightNeighbor = j < extendedCells.size() - 1 ? extendedCells[j + 1] : 0;
            std::string neighborhood = std::to_string(leftNeighbor) + std::to_string(currentCell) + std::to_string(rightNeighbor);
            nextGeneration[j] = calculateCell(neighborhood, ruleBinary);
        }
        cells = nextGeneration;
    }

    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> duration = end - start;
    std::cout << "Took " << duration.count() << "ms to generate " << generations << " generations of rule " << ruleNumber << std::endl;

    return automatonData;
}

void outputToFile(const std::vector<std::vector<int>> &automatonData, const int ruleNumber, const int generations, const std::string &initialConditions) {
    int imageWidth = automatonData.empty() ? 0 : automatonData[0].size();

    std::ofstream file("results/r" + std::to_string(ruleNumber) + "_g" + std::to_string(generations) + "_i" + initialConditions + "_cpp.pbm");
    file << "P1\n" << imageWidth << " " << generations << "\n";

    for (const auto &row : automatonData) {
        for (int cell : row) {
            file << (cell ? '1' : '0');
        }
        file << '\n';
    }

    file.close();
}

int main() {
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input file!" << std::endl;
        return 1;
    }

    int ruleNumber;
    std::string initialConditions;
    int generations;

    inputFile >> ruleNumber;
    inputFile >> initialConditions;
    inputFile >> generations;

    auto start = std::chrono::high_resolution_clock::now();

    std::vector<std::vector<int>> automatonData = runCellularAutomaton(ruleNumber, generations, initialConditions, start);
    outputToFile(automatonData, ruleNumber, generations, initialConditions);

    std::cout << "Done!" << std::endl;

    return 0;
}
