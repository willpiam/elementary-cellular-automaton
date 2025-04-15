#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LENGTH_INITIAL_CONDITIONS 1000

// Convert rule number to binary representation
bool* ruleToBinaryArray(int ruleNumber) {
    bool* ruleBinary = (bool*)malloc(8 * sizeof(bool));
    if (!ruleBinary) {
        return NULL;
    }
    
    for (int i = 0; i < 8; i++) {
        ruleBinary[i] = (ruleNumber >> i) & 1;
    }
    return ruleBinary;
}

// Calculate new cell state based on neighborhood and rule
bool calculateCell(bool left, bool center, bool right, bool* ruleBinary) {
    int index = (left ? 4 : 0) | (center ? 2 : 0) | (right ? 1 : 0);
    return ruleBinary[index];
}

// Run cellular automaton simulation
bool** runCellularAutomaton(int ruleNumber, int generations, const char* initialConditions) {
    int initialConditionsLength = strlen(initialConditions);
    int imageWidth = initialConditionsLength + 2 * generations;
    bool** automatonData = NULL;
    bool* ruleBinary = ruleToBinaryArray(ruleNumber);
    
    if (!ruleBinary) {
        printf("Error allocating memory for rule!\n");
        return NULL;
    }
    
    // Allocate memory for automaton data (2D array of bools)
    automatonData = (bool**)malloc(generations * sizeof(bool*));
    if (!automatonData) {
        printf("Error allocating memory for automaton data!\n");
        free(ruleBinary);
        return NULL;
    }
    
    // Allocate memory for each generation
    for (int i = 0; i < generations; i++) {
        automatonData[i] = (bool*)calloc(imageWidth, sizeof(bool));
        if (!automatonData[i]) {
            printf("Error allocating memory for generation %d!\n", i);
            // Clean up already allocated memory
            for (int j = 0; j < i; j++) {
                free(automatonData[j]);
            }
            free(automatonData);
            free(ruleBinary);
            return NULL;
        }
    }
    
    // Initialize first generation with initial conditions
    int initialOffset = (imageWidth - initialConditionsLength) / 2;
    for (int j = 0; j < initialConditionsLength; j++) {
        automatonData[0][j + initialOffset] = (initialConditions[j] == '1');
    }
    
    // Generate subsequent generations
    int length = initialConditionsLength + 2;
    for (int i = 1; i < generations; i++) {
        int paddingOffset = initialOffset - i;
        for (int j = paddingOffset; j < paddingOffset + length; j++) {
            bool left = automatonData[i-1][j-1];
            bool center = automatonData[i-1][j];
            bool right = automatonData[i-1][j+1];
            automatonData[i][j] = calculateCell(left, center, right, ruleBinary);
        }
        length += 2;
    }
    
    free(ruleBinary);
    return automatonData;
}

// Write automaton data to PBM file
int outputToFile(bool** automatonData, int ruleNumber, int generations, const char* initialConditions) {
    int initialConditionsLength = strlen(initialConditions);
    int imageWidth = initialConditionsLength + 2 * generations;
    
    char filename[MAX_LENGTH_INITIAL_CONDITIONS + 50];
    sprintf(filename, "results/r%d_g%d_i%s_c.pbm", 
            ruleNumber, generations, initialConditions);
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        return 1;
    }
    
    // Write PBM header
    fprintf(file, "P1\n%d %d\n", imageWidth, generations);
    
    // Write automaton data
    for (int i = 0; i < generations; i++) {
        for (int j = 0; j < imageWidth; j++) {
            fputc(automatonData[i][j] ? '1' : '0', file);
        }
        fputc('\n', file);
    }
    
    fclose(file);
    return 0;
}

// Main function
int main() {
   
    FILE* inputFile = fopen("input.txt", "r");
    if (!inputFile) {
        printf("Error opening input file!\n");
        return 1;
    }
    
    int ruleNumber, generations;
    char initialConditions[MAX_LENGTH_INITIAL_CONDITIONS];
    
    // Read input file
    if (fscanf(inputFile, "%d %s %d", &ruleNumber, initialConditions, &generations) != 3) {
        printf("Error reading input file! Expected 3 values.\n");
        fclose(inputFile);
        return 1;
    }
    
    fclose(inputFile);
    
    // Run cellular automaton
    bool** automatonData = runCellularAutomaton(ruleNumber, generations, initialConditions);
    if (!automatonData) {
        printf("Failed to run cellular automaton!\n");
        return 1;
    }
    
    // Output results to file
    int resultFlag = outputToFile(automatonData, ruleNumber, generations, initialConditions);
    
    // Clean up memory
    for (int i = 0; i < generations; i++) {
        free(automatonData[i]);
    }
    free(automatonData);
    
    if (resultFlag != 0) {
        printf("Error writing to file!\n");
        return 1;
    }
    
    printf("Cellular automaton successfully generated and saved.\n");
    return 0;
}