#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int* ruleToBinaryArray(int ruleNumber) {
    int* ruleBinary = (int*)malloc(8 * sizeof(int));
    for (int i = 0; i < 8; i++) {
        ruleBinary[i] = (ruleNumber >> i) & 1;
    }
    return ruleBinary;
}

int calculateCell(const char *neighborhood, const int *ruleBinary) {
    int index = strtol(neighborhood, NULL, 2);
    return ruleBinary[index];
}

int** runCellularAutomaton(const int ruleNumber, const int generations, const char *initialConditions, clock_t start) {
    int length = strlen(initialConditions);
    int* cells = (int*)malloc(length * sizeof(int));
    for (int i = 0; i < length; i++) {
        cells[i] = initialConditions[i] == '1' ? 1 : 0;
    }

    int* ruleBinary = ruleToBinaryArray(ruleNumber);
    int imageWidth = length + 2 * generations;
    int** automatonData = (int**)malloc(generations * sizeof(int*));

    for (int i = 0; i < generations; i++) {
        int paddingLength = (imageWidth - length) / 2;
        int* extendedCells = (int*)malloc(imageWidth * sizeof(int));
        memset(extendedCells, 0, imageWidth * sizeof(int));
        memcpy(extendedCells + paddingLength, cells, length * sizeof(int));

        automatonData[i] = extendedCells;

        int* nextGeneration = (int*)malloc(imageWidth * sizeof(int));
        for (int j = 0; j < imageWidth; j++) {
            int leftNeighbor = j > 0 ? extendedCells[j - 1] : 0;
            int currentCell = extendedCells[j];
            int rightNeighbor = j < imageWidth - 1 ? extendedCells[j + 1] : 0;
            char neighborhood[4];
            sprintf(neighborhood, "%d%d%d", leftNeighbor, currentCell, rightNeighbor);
            nextGeneration[j] = calculateCell(neighborhood, ruleBinary);
        }
        free(cells);
        cells = nextGeneration;
        length = imageWidth; // Update the length of cells array

        if (i + 1 == generations) {
            free(nextGeneration);
        }
    }


    clock_t end = clock();
    double duration = ((double)(end - start)) / CLOCKS_PER_SEC * 1000;
    printf("Took %fms to generate %d generations of rule %d\n", duration, generations, ruleNumber);

    free(ruleBinary);
    return automatonData;
}

void outputToFile(int** automatonData, int ruleNumber, int generations, const char *initialConditions, int imageWidth) {
    char filename[100];
    sprintf(filename, "results/r%d_g%d_i%s_c.pbm", ruleNumber, generations, initialConditions);
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        return;
    }

    fprintf(file, "P1\n%d %d\n", imageWidth, generations);
    for (int i = 0; i < generations; i++) {
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
    int** automatonData = runCellularAutomaton(ruleNumber, generations, initialConditions, start);
    int imageWidth = strlen(initialConditions) + 2 * generations;
    outputToFile(automatonData, ruleNumber, generations, initialConditions, imageWidth);
    free(automatonData);

    printf("Done!\n");

    return 0;
}
