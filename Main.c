#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/types.h>

#define MAX_INIT_LEN 1000

// Inline rule application for one cell
static inline uint8_t apply_rule(const uint8_t *restrict prev, int j, const uint8_t rule[8]) {
    int idx = (prev[j-1] << 2) | (prev[j] << 1) | prev[j+1];
    return rule[idx];
}

int main(void) {
    // Read input values from file
    FILE *input = fopen("input.txt", "r");
    if (!input) {
        perror("Error opening input.txt");
        return 1;
    }

    int ruleNumber, generations;
    char initial[MAX_INIT_LEN + 1];
    if (fscanf(input, "%d %1000s %d", &ruleNumber, initial, &generations) != 3) {
        fprintf(stderr, "Error: expected rule, initial string, and generations in input.txt\n");
        fclose(input);
        return 1;
    }
    fclose(input);

    int initLen = strlen(initial);
    int width = initLen + 2 * generations;

    // Prepare rule array on stack
    uint8_t ruleBinary[8];
    for (int i = 0; i < 8; i++) {
        ruleBinary[i] = (ruleNumber >> i) & 1;
    }

    // Allocate two rows and a line buffer
    uint8_t *prev = calloc(width, 1);
    uint8_t *curr = calloc(width, 1);
    char    *lineBuf = malloc(width + 1);
    if (!prev || !curr || !lineBuf) {
        perror("Memory allocation failed");
        return 1;
    }

    // Center initial pattern in the 'prev' buffer
    int offset = generations;
    for (int i = 0; i < initLen; i++) {
        prev[offset + i] = (initial[i] == '1');
    }

    // Ensure output directory exists
    mkdir("results", 0755);

    // Open PBM output file
    char filename[MAX_INIT_LEN + 50];
    snprintf(filename, sizeof(filename), "results/r%d_g%d_i%s_c.pbm", ruleNumber, generations, initial);
    FILE *out = fopen(filename, "w");
    if (!out) {
        perror("Error creating output file");
        return 1;
    }

    // Write PBM header
    fprintf(out, "P1\n%d %d\n", width, generations);

    // Main generation loop: write each row then compute next
    for (int g = 0; g < generations; ++g) {
        // Serialize current row to ASCII in one buffer
        for (int j = 0; j < width; ++j) {
            lineBuf[j] = prev[j] ? '1' : '0';
        }
        lineBuf[width] = '\n';
        fwrite(lineBuf, 1, width + 1, out);

        // Compute next generation into 'curr'
        for (int j = 1; j < width - 1; ++j) {
            curr[j] = apply_rule(prev, j, ruleBinary);
        }
        // Edges remain zero

        // Swap buffers
        uint8_t *tmp = prev;
        prev = curr;
        curr = tmp;
    }

    // Clean up
    fclose(out);
    free(prev);
    free(curr);
    free(lineBuf);

    return 0;
}