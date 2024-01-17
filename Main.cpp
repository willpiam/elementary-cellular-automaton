#include <iostream>
#include <fstream>
#include <vector>
#include <bitset>
#include <string>
#include <chrono>

std::vector<int> ruleToBinaryArray(const int ruleNumber) {
    std::bitset<8> binary(ruleNumber);
    std::vector<int> ruleBinary(8);

    for (int i = 0; i < 8; i++) 
        ruleBinary[i] = binary[i];

    return ruleBinary;
}

int calculateCell(const std::string &neighborhood, const std::vector<int> &ruleBinary) {
    int index = std::stoi(neighborhood, nullptr, 2);
    return ruleBinary[index];
}

std::vector<std::vector<int>> runCellularAutomaton(const int ruleNumber, const int generations, const std::string &initialConditions) {
    std::vector<int> cells;
    for (char bit : initialConditions) 
        cells.push_back(bit == '1' ? 1 : 0);

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


    return automatonData;
}

// void outputToFile(const std::vector<std::vector<int>> &automatonData, const int ruleNumber, const int generations, const std::string &initialConditions) {
//     int imageWidth = automatonData.empty() ? 0 : automatonData[0].size();

//     std::ofstream file("results/r" + std::to_string(ruleNumber) + "_g" + std::to_string(generations) + "_i" + initialConditions + "_cpp.pbm");
//     file << "P1\n" << imageWidth << " " << generations << "\n";

//     for (const auto &row : automatonData) {
//         for (int cell : row) 
//             file << (cell ? '1' : '0');
//         file << '\n';
//     }

//     file.close();
// }
void outputToFile(const std::vector<std::vector<int>> &automatonData, const int ruleNumber, const int generations, const std::string &initialConditions) {
    if (automatonData.empty()) return;

    int imageWidth = automatonData[0].size();
    std::string output = "P1\n" + std::to_string(imageWidth) + " " + std::to_string(generations) + "\n";

    for (const auto &row : automatonData) {
        for (int cell : row) {
            output += (cell ? '1' : '0');
        }
        output += '\n';
    }

    std::ofstream file("results/r" + std::to_string(ruleNumber) + "_g" + std::to_string(generations) + "_i" + initialConditions + "_cpp.pbm");
    file << output;
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

    std::vector<std::vector<int>> automatonData = runCellularAutomaton(ruleNumber, generations, initialConditions);

    auto endCalculation = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> durationOfCalculation = endCalculation - start;
    std::cout << "Took " << durationOfCalculation.count() << "ms to generate " << generations << " generations of rule " << ruleNumber << std::endl;

    outputToFile(automatonData, ruleNumber, generations, initialConditions);
    
    auto endOutput = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double, std::milli> durationOfOutput = endOutput - endCalculation;
    std::cout << "Took " << durationOfOutput.count() << "ms to save results" << std::endl; 

    std::chrono::duration<double, std::milli> durationWhole = endOutput - start;
    std::cout << "Took " << durationWhole.count() << "ms in total" << std::endl;

    std::cout << "Done!" << std::endl;

    return 0;
}
