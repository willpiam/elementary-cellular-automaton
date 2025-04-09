#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <array> 

std::array<int, 8> rule_to_binary_array(const int rule_number) {
    std::array<int, 8> result;
    for (int i = 0; i < 8; ++i) 
        result[7 - i] = (rule_number >> i) & 1;
    return result;
}

int calculate_cell(const int left, const int center, const int right, const std::array<int, 8>& rule) {
    int pattern = (left << 2) | (center << 1) | right;
    return rule[7 - pattern];
}

std::vector<std::vector<int>> run_cellular_automaton(const std::array<int, 8>& rule, const int generations, const std::vector<int>& initial_cells) {
    std::vector<int> cells = initial_cells;
    std::vector<std::vector<int>> ca;
    ca.reserve(generations);  // Pre-allocate space for all generations

    for (int i = 0; i < generations - 1; ++i) {
        std::vector<int> extended_cells = {0, 0};
        extended_cells.insert(extended_cells.end(), cells.begin(), cells.end());
        extended_cells.push_back(0);
        extended_cells.push_back(0);

        ca.push_back(cells);

        std::vector<int> next_generation;
        next_generation.reserve(cells.size());  // Pre-allocate space for next generation

        for (size_t j = 1; j < extended_cells.size() - 1; ++j) {
            next_generation.push_back(calculate_cell(
                extended_cells[j - 1],
                extended_cells[j],
                extended_cells[j + 1],
                rule
            ));
        }

        cells = std::move(next_generation);  // Use move semantics
    }

    ca.push_back(cells);
    return ca;
}

std::vector<std::vector<int>> pad_image_data(const std::vector<std::vector<int>>& image_data, const int total_width) {
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
    int final_width = initial_conditions.size() + 2 * generations;
    auto padded_ca = pad_image_data(ca, final_width);

    std::ofstream file("results/r" + std::to_string(rule_number) + "_g" + std::to_string(generations) + "_i" + initial_conditions + "_cpp.pbm");
    if (file.is_open()) {
        file << "P1\n" << final_width << " " << generations << "\n";
        for (const auto& row : padded_ca) {
            for (int cell : row) {
                file << cell;
            }
            file << "\n";
        }
    }

    return 0;
}
