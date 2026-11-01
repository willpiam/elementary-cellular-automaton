#include <cstdint>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

static void die(const char *msg) {
    std::cerr << msg << '\n';
    std::exit(1);
}

static void trim(std::string &s) {
    size_t start = 0;
    while (start < s.size() && (s[start] == ' ' || s[start] == '\t'))
        start++;
    size_t end = s.size();
    while (end > start && (s[end - 1] == ' ' || s[end - 1] == '\t' || s[end - 1] == '\r'))
        end--;
    s = s.substr(start, end - start);
}

static bool parse_long(const std::string &s, long &out) {
    if (s.empty())
        return false;
    char *end = nullptr;
    long v = std::strtol(s.c_str(), &end, 10);
    if (end == s.c_str() || *end != '\0')
        return false;
    out = v;
    return true;
}

static bool valid_seed(const std::string &s) {
    if (s.empty())
        return false;
    for (char c : s) {
        if (c != '0' && c != '1')
            return false;
    }
    return true;
}

int main() {
    std::ifstream input("input.txt");
    if (!input)
        die("cannot read input.txt");

    std::string rule_line, seed, gen_line;
    if (!std::getline(input, rule_line) || !std::getline(input, seed) || !std::getline(input, gen_line))
        die("invalid input");
    if (!seed.empty() && seed.back() == '\r')
        seed.pop_back();
    trim(rule_line);
    trim(gen_line);

    long rule_number = 0, generations = 0;
    if (!parse_long(rule_line, rule_number) || rule_number < 0 || rule_number > 255)
        die("invalid rule");
    if (!valid_seed(seed))
        die("invalid initial conditions");
    if (!parse_long(gen_line, generations) || generations < 1)
        die("invalid generations");

    const long n = static_cast<long>(seed.size());
    if (generations > (INT32_MAX - n) / 2)
        die("invalid generations");

    const int width = static_cast<int>(n + 2 * generations);
    const int g = static_cast<int>(generations);
    const int init_len = static_cast<int>(n);
    const bool even = (rule_number & 1) == 0;

    uint8_t rule[8];
    for (int i = 0; i < 8; i++)
        rule[i] = static_cast<uint8_t>((rule_number >> i) & 1);

    std::vector<uint8_t> prev(width, 0), next(width, 0);
    for (int i = 0; i < init_len; i++)
        prev[g + i] = seed[i] == '1' ? 1 : 0;

    if (std::system("mkdir -p results") != 0)
        die("cannot create results");

    const std::string path = "results/r" + std::to_string(rule_number) + "_g" + std::to_string(generations) + "_i" + seed + "_cpp.pbm";
    std::ofstream out(path, std::ios::binary);
    if (!out)
        die("cannot write output");

    out << "P1\n" << width << ' ' << g << '\n';
    std::string line(static_cast<size_t>(width) + 1, '0');
    line[width] = '\n';

    for (int gen = 0; gen < g; gen++) {
        for (int j = 0; j < width; j++)
            line[j] = prev[j] ? '1' : '0';
        out.write(line.data(), line.size());
        if (!out)
            die("cannot write output");
        if (gen + 1 == g)
            break;

        std::fill(next.begin(), next.end(), 0);
        int lo, hi;
        if (even) {
            lo = g - (gen + 1);
            hi = g + init_len + (gen + 1);
        } else {
            lo = 0;
            hi = width;
        }
        for (int j = lo; j < hi; j++) {
            int left = j > 0 ? prev[j - 1] : 0;
            int right = j + 1 < width ? prev[j + 1] : 0;
            int idx = (left << 2) | (prev[j] << 1) | right;
            next[j] = rule[idx];
        }
        std::swap(prev, next);
    }
    return 0;
}
