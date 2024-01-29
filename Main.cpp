// #include <iostream>
// #include <fstream>
// #include <vector>
// #include <bitset>
// #include <string>
// #include <chrono>

// std::vector<int> ruleToBinaryArray(const int ruleNumber) {
//     std::bitset<8> binary(ruleNumber);
//     std::vector<int> ruleBinary(8);

//     for (int i = 0; i < 8; i++) 
//         ruleBinary[i] = binary[i];

//     return ruleBinary;
// }

// int calculateCell(const std::string &neighborhood, const std::vector<int> &ruleBinary) {
//     int index = std::stoi(neighborhood, nullptr, 2);
//     return ruleBinary[index];
// }

// std::vector<std::vector<int>> runCellularAutomaton(const int ruleNumber, const int generations, const std::string &initialConditions) {
//     std::vector<int> cells;
//     for (char bit : initialConditions) 
//         cells.push_back(bit == '1' ? 1 : 0);

//     std::vector<int> ruleBinary = ruleToBinaryArray(ruleNumber);
//     int imageWidth = cells.size() + 2 * generations;
//     std::vector<std::vector<int>> automatonData;

//     for (int i = 0; i < generations; i++) {
//         int paddingLength = (imageWidth - cells.size()) / 2;
//         std::vector<int> padding(paddingLength, 0);
//         std::vector<int> extendedCells(padding);
//         extendedCells.insert(extendedCells.end(), cells.begin(), cells.end());
//         extendedCells.insert(extendedCells.end(), padding.begin(), padding.end());

//         automatonData.push_back(extendedCells);

//         std::vector<int> nextGeneration(extendedCells.size());
//         for (size_t j = 0; j < extendedCells.size(); j++) {
//             int leftNeighbor = j > 0 ? extendedCells[j - 1] : 0;
//             int currentCell = extendedCells[j];
//             int rightNeighbor = j < extendedCells.size() - 1 ? extendedCells[j + 1] : 0;
//             std::string neighborhood = std::to_string(leftNeighbor) + std::to_string(currentCell) + std::to_string(rightNeighbor);
//             nextGeneration[j] = calculateCell(neighborhood, ruleBinary);
//         }
//         cells = nextGeneration;
//     }


//     return automatonData;
// }

// void outputToFile(const std::vector<std::vector<int>> &automatonData, const int ruleNumber, const int generations, const std::string &initialConditions) {
//     if (automatonData.empty()) return;

//     int imageWidth = automatonData[0].size();
//     std::string output = "P1\n" + std::to_string(imageWidth) + " " + std::to_string(generations) + "\n";

//     for (const auto &row : automatonData) {
//         for (int cell : row) 
//             output += (cell ? '1' : '0');
//         output += '\n';
//     }

//     std::ofstream file("results/r" + std::to_string(ruleNumber) + "_g" + std::to_string(generations) + "_i" + initialConditions + "_cpp.pbm");
//     file << output;
//     file.close();
// }

// int main() {
//     std::ifstream inputFile("input.txt");
//     if (!inputFile.is_open()) {
//         std::cerr << "Error opening input file!" << std::endl;
//         return 1;
//     }

//     int ruleNumber;
//     std::string initialConditions;
//     int generations;

//     inputFile >> ruleNumber;
//     inputFile >> initialConditions;
//     inputFile >> generations;

//     auto start = std::chrono::high_resolution_clock::now();

//     std::vector<std::vector<int>> automatonData = runCellularAutomaton(ruleNumber, generations, initialConditions);

//     auto endCalculation = std::chrono::high_resolution_clock::now();
//     std::chrono::duration<double, std::milli> durationOfCalculation = endCalculation - start;
//     std::cout << "Took " << durationOfCalculation.count() << "ms to generate " << generations << " generations of rule " << ruleNumber << std::endl;

//     outputToFile(automatonData, ruleNumber, generations, initialConditions);
    
//     auto endOutput = std::chrono::high_resolution_clock::now();
//     std::chrono::duration<double, std::milli> durationOfOutput = endOutput - endCalculation;
//     std::cout << "Took " << durationOfOutput.count() << "ms to save results" << std::endl; 

//     std::chrono::duration<double, std::milli> durationWhole = endOutput - start;
//     std::cout << "Took " << durationWhole.count() << "ms in total" << std::endl;

//     std::cout << "Done!" << std::endl;

//     return 0;
// }
 // below works but is too slow
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <bitset>
#include <chrono>

std::vector<int> rule_to_binary_array(int rule_number) {
    std::bitset<8> binary(rule_number);
    std::vector<int> result;
    for (int i = 7; i >= 0; --i) {
        result.push_back(binary[i]);
    }
    return result;
}

int calculate_cell(const std::string& p_state, const std::vector<int>& rule) {
    std::vector<std::string> states = {"111", "110", "101", "100", "011", "010", "001", "000"};
    for (size_t i = 0; i < states.size(); ++i) {
        if (p_state == states[i]) {
            return rule[i];
        }
    }
    return 0;
}

std::vector<std::vector<int>> run_cellular_automaton(const std::vector<int>& rule, int generations, const std::vector<int>& initial_cells) {
    std::vector<int> cells = initial_cells;
    std::vector<std::vector<int>> ca;

    for (int i = 0; i < generations - 1; ++i) {
        std::vector<int> extended_cells = {0, 0};
        extended_cells.insert(extended_cells.end(), cells.begin(), cells.end());
        extended_cells.push_back(0);
        extended_cells.push_back(0);

        ca.push_back(cells);

        std::vector<int> next_generation;
        for (size_t j = 1; j < extended_cells.size() - 1; ++j) {
            std::string neighborhood = std::to_string(extended_cells[j - 1]) + std::to_string(extended_cells[j]) + std::to_string(extended_cells[j + 1]);
            next_generation.push_back(calculate_cell(neighborhood, rule));
        }
        cells = next_generation;
    }
    ca.push_back(cells);
    return ca;
}

std::vector<std::vector<int>> pad_image_data(const std::vector<std::vector<int>>& image_data, int total_width) {
    std::vector<std::vector<int>> padded_data;
    for (const auto& row : image_data) {
        int padding_length = (total_width - row.size()) / 2;
        std::vector<int> padded_row(padding_length, 0);
        padded_row.insert(padded_row.end(), row.begin(), row.end());
        padded_row.insert(padded_row.end(), padding_length, 0);
        padded_data.push_back(padded_row);
    }
    return padded_data;
}

void read_inputs_from_file(const std::string& file_path, int& rule_number, std::string& initial_conditions, int& generations) {
    std::ifstream file(file_path);
    if (file.is_open()) {
        file >> rule_number;
        file.ignore();
        std::getline(file, initial_conditions);
        file >> generations;
    }
}

int main() {
    int rule_number, generations;
    std::string initial_conditions;
    read_inputs_from_file("input.txt", rule_number, initial_conditions, generations);

    std::cout << "Rule Number: " << rule_number << std::endl;
    std::cout << "Initial Conditions: " << initial_conditions << std::endl;
    std::cout << "Generations: " << generations << std::endl;

    std::vector<int> rule_binary = rule_to_binary_array(rule_number);
    std::vector<int> cells;
    for (char c : initial_conditions) {
        cells.push_back(c - '0');
    }

    auto start_time = std::chrono::high_resolution_clock::now();

    std::vector<std::vector<int>> ca = run_cellular_automaton(rule_binary, generations, cells);

    int final_width = initial_conditions.length() + 2 * generations;
    std::vector<std::vector<int>> padded_ca = pad_image_data(ca, final_width);

    std::string image_data = "P1\n" + std::to_string(final_width) + " " + std::to_string(generations) + "\n";
    for (const auto& row : padded_ca) {
        for (int num : row) {
            image_data += std::to_string(num);
        }
        image_data += "\n";
    }

    std::ofstream file("results/r" + std::to_string(rule_number) + "_g" + std::to_string(generations) + "_i" + initial_conditions + "_cpp.pbm");
    if (file.is_open()) {
        file << image_data;
    }

    auto end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end_time - start_time;
    std::cout << "Took " << elapsed.count() << "s to generate " << generations << " generations of rule " << rule_number << std::endl;

    return 0;
}
