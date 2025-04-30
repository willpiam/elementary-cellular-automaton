#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <array> 

std::array<int, 8> rule_to_binary_array(const int rule_number) {
    std::array<int, 8> result;
    for (int i = 0; i < 8; ++i)
        result[i] = (rule_number >> i) & 1;
    return result;
}

int calculate_cell(const int left, const int center, const int right, const std::array<int, 8>& rule) {
    int pattern = (left << 2) | (center << 1) | right;
    return rule[pattern];
}

std::vector<std::vector<int>> run_cellular_automaton(const std::array<int, 8>& rule, const int generations, const std::vector<int>& initial_cells) {
    int width = static_cast<int>(initial_cells.size()) + 2 * generations;
    std::vector<std::vector<int>> ca(generations, std::vector<int>(width, 0));
    int offset = generations;
    // initialize first generation
    for (size_t i = 0; i < initial_cells.size(); ++i)
        ca[0][offset + i] = initial_cells[i];
    // compute subsequent generations
    for (int g = 1; g < generations; ++g) {
        const auto& prev = ca[g - 1];
        auto& curr = ca[g];
        for (int j = 0; j < width; ++j) {
            int left = (j > 0) ? prev[j - 1] : 0;
            int center = prev[j];
            int right = (j + 1 < width) ? prev[j + 1] : 0;
            curr[j] = rule[(left << 2) | (center << 1) | right];
        }
    }
    return ca;
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
    int rule_number;
    std::string initial_conditions;
    int generations;
    read_inputs_from_file("input.txt", rule_number, initial_conditions, generations);

    auto rule_binary = rule_to_binary_array(rule_number);
    std::vector<int> cells;
    for (char c : initial_conditions) {
        cells.push_back(c - '0');
    }

    auto ca = run_cellular_automaton(rule_binary, generations, cells);
    int final_width = static_cast<int>(initial_conditions.size()) + 2 * generations;
    std::ofstream file("results/r" + std::to_string(rule_number) + "_g" + std::to_string(generations) + "_i" + initial_conditions + "_cpp.pbm");
    if (file.is_open()) {
        file << "P1\n" << final_width << " " << generations << "\n";
        for (const auto& row : ca) {
            for (int cell : row) {
                file << cell;
            }
            file << "\n";
        }
    }

    return 0;
}
