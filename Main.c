#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// #define EXPERIMENTAL

char* ruleToBinaryArray(char ruleNumber) {
    char* ruleBinary = (char*)malloc(8 * sizeof(char));
    for (char i = 0; i < 8; i++) {
        ruleBinary[i] = (ruleNumber >> i) & 1;
    }
    return ruleBinary;
}

char calculateCell(const char *neighborhood, const char* ruleBinary) {
    char index = strtol(neighborhood, NULL, 2);
    return ruleBinary[index];
}

void printBinaryString(const char* data, size_t length) {
    for (size_t i = 0; i < length; i++) {
        // Check if the value is 1 or 0 and print the corresponding ASCII character.
        printf("%c", data[i] ? '1' : '0');
    }
    printf("\n"); // Print a newline at the end for readability.
}

#ifdef EXPERIMENTAL

char** runCellularAutomaton(const char* rule, const int generations, const char *initialConditions) {
    int length = strlen(initialConditions);
    char* cells = (char*)malloc(length * sizeof(char));

    for (int i = 0; i < length; i++) {
        cells[i] = initialConditions[i] == '1' ? 1 : 0;
    }

    const int imageWidth = length + 2 * generations;
    char** automatonData = (char**)malloc((generations + 1) * sizeof(char*));

    for (int gen = 0; gen <= generations; gen++) {
        char* nextGeneration = (char*)malloc(length + 2 * sizeof(char));                    // allocate memory for the next generation (each generation is 2 cells longer than the previous one)
        memset(nextGeneration, 0, (length + 2) * sizeof(char));                             // fill the active cells with zeros

        for (int j = 0; j < length; j++) {                                                  // iterate over the active cells
            char leftNeighbor = j > 0 ? cells[j - 1] : 0;
            char currentCell = cells[j];
            char rightNeighbor = j < length - 1 ? cells[j + 1] : 0;
            char neighborhood[4];
            sprintf(neighborhood, "%d%d%d", leftNeighbor, currentCell, rightNeighbor);
            nextGeneration[j + 1] = calculateCell(neighborhood, rule); // Shift by 1 to account for padding on the left
        }

        length += 2; // Update the length for the next iteration
        const int paddingLength = ((imageWidth - length) / 2);                              // number of zeros before first potentially active cell
        char* extendedCells = (char*)malloc(imageWidth * sizeof(char));                     // allocate memory for a full row (not just the active cells)
        memset(extendedCells, 0, imageWidth * sizeof(char));                                // fill the row with zeros
        memcpy(extendedCells + paddingLength, nextGeneration, length * sizeof(char));                // copy the active cells into the middle of the row

        printf("%d. ", gen);                                                                // print the generation number (for debugging purposes
        printBinaryString(extendedCells, imageWidth);
        automatonData[gen] = extendedCells;                                                 // point to the row from the CA
        free(cells);
        cells = nextGeneration;
    }
 
    return automatonData;
}

#else

char** runCellularAutomaton(const char* rule, const int generations, const char *initialConditions) {
    int length = strlen(initialConditions);
    char* cells = (char*)malloc(length * sizeof(char));

    for (int i = 0; i < length; i++) {
        // cells[i] = initialConditions[i] == '1' ? 1 : 0;
        cells[i] = initialConditions[i] - '0';
    }

    const int imageWidth = length + 2 * generations;
    char** automatonData = (char**)malloc(generations * sizeof(char*));

    for (int i = 0; i < generations; i++) {
        int paddingLength = (imageWidth - length) / 2;
        char* extendedCells = (char*)malloc(imageWidth * sizeof(char));
        memset(extendedCells, 0, imageWidth * sizeof(char));
        memcpy(extendedCells + paddingLength, cells, length * sizeof(char));

        automatonData[i] = extendedCells;

        char* nextGeneration = (char*)malloc(imageWidth * sizeof(char));
        for (int j = 0; j < imageWidth; j++) {
            int leftNeighbor = j > 0 ? extendedCells[j - 1] : 0;
            int currentCell = extendedCells[j];
            int rightNeighbor = j < imageWidth - 1 ? extendedCells[j + 1] : 0;
            char neighborhood[4];
            sprintf(neighborhood, "%d%d%d", leftNeighbor, currentCell, rightNeighbor);
            nextGeneration[j] = calculateCell(neighborhood, rule);
        }
        free(cells);
        cells = nextGeneration;
        length = imageWidth; // Update the length of cells array

        if (i + 1 == generations) {
            free(nextGeneration);
        }
    }

    return automatonData;
}

#endif

void outputToFile(char** automatonData, int ruleNumber, int generations, const char *initialConditions, int imageWidth) {
    char filename[100];
    sprintf(filename, "results/r%d_g%d_i%s_c.pbm", ruleNumber, generations, initialConditions);
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        return;
    }

    fprintf(file, "P1\n%d %d\n", imageWidth, generations);
    for (int i = 0; i < generations; i++) {
        int generationSize = strlen(automatonData[i]);
        for (int j = 0; j < imageWidth; j++) {
            fprintf(file, "%d", automatonData[i][j]);
        }
        fprintf(file, "\n");
        free(automatonData[i]);
    }
    fclose(file);
}

int main() {
    FILE *inputFile = fopen("input.txt", "r");
    if (!inputFile) {
        printf("Error opening input file!\n");
        return 1;
    }

    int ruleNumber, generations;
    char initialConditions[100];
    fscanf(inputFile, "%d %s %d", &ruleNumber, initialConditions, &generations);
    fclose(inputFile);

    clock_t start = clock();
    char* rule = ruleToBinaryArray(ruleNumber);
    char** automatonData = runCellularAutomaton(rule, generations, initialConditions);

    int imageWidth = strlen(initialConditions) + 2 * generations;
    outputToFile(automatonData, ruleNumber, generations, initialConditions, imageWidth);
    
    clock_t end = clock();
    double duration = ((double)(end - start)) / CLOCKS_PER_SEC * 1000;
    printf("Took %fms to generate %d generations of rule %d\n", duration, generations, ruleNumber);

    free(rule);
    free(automatonData);


    printf("Done!\n");

    return 0;
}