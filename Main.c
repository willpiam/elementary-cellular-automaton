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

char** runCellularAutomaton(const int* rule, const int generations, const char *initialConditions) {
    int length = strlen(initialConditions);
    char* cells = (char*)malloc(length * sizeof(char));

    for (int i = 0; i < length; i++) {
        cells[i] = initialConditions[i] == '1' ? 1 : 0;
    }

    int imageWidth = length + 2 * generations;
    char** automatonData = (char**)malloc(generations * sizeof(char*));

    for (int i = 0; i < generations; i++) {
        int paddingLength = (imageWidth - length) / 2;
        char* extendedCells = (char*)malloc(imageWidth * sizeof(char));
        memset(extendedCells, 0, imageWidth * sizeof(char));
        memcpy(extendedCells + paddingLength, cells, length * sizeof(char));

        // char* extendedCells = (char*)malloc((length + 4) * sizeof(char));
        // extendedCells[0] = 0;
        // extendedCells[1] = 0;
        // strcpy(extendedCells + 2, cells);
        // extendedCells[length + 2] = 0;
        // extendedCells[length + 3] = 0;
        
        printf("extendedCells: %s\n", extendedCells);

        automatonData[i] = extendedCells;

        char* nextGeneration = (char*)malloc(imageWidth * sizeof(char));
        // char* nextGeneration = (char*)malloc((length + 2) * sizeof(char));

        // for (int j = 0; j < (length + 4) -1; j++) {
        for (int j = 0; j < imageWidth; j++) {
            char leftNeighbor = j > 0 ? extendedCells[j - 1] : 0;
            char currentCell = extendedCells[j];
            char rightNeighbor = j < imageWidth - 1 ? extendedCells[j + 1] : 0;
            char neighborhood[4];
            sprintf(neighborhood, "%d%d%d", leftNeighbor, currentCell, rightNeighbor);
            // for (int k = -1; k < 2; k++) {
            //     neighborhood[k] = extendedCells[j + k];
            // }
            nextGeneration[j] = calculateCell(neighborhood, rule);
        }
        free(cells);
        cells = nextGeneration;
        length = imageWidth; // Update the length of cells array
        // length += 2;

        if (i + 1 == generations) {
            free(nextGeneration);
        }
    }

    return automatonData;
}

// char** padCellularAutomata(char** automatonData, int generations, int initialConditionsLength) {
//     char** paddedCA = (char**)malloc(generations * sizeof(char*));
//     int totalWidth = initialConditionsLength + 2 * generations;

//     for (int i = 0; i < generations; i++) {
//         paddedCA[i] = (char*)malloc(totalWidth * sizeof(char));
        
//         // int generationSize = strlen(automatonData[i]) + 2 *( i + 1);
//         int generationSize = strlen(automatonData[i]);
//         printf("generationSize: %d\n", generationSize);
//         int paddingLength = (totalWidth - generationSize) / 2;
//         // printf("paddingLength: %d\n", paddingLength);

//         // // insert the data directly into the padded array
//         int cellsThisGeneration = initialConditionsLength + 2 * i;
        
//         for (int j = 0; j < cellsThisGeneration; j++) {
//             printf("\ti: %d, j: %d\n", i, j);
//             // paddedCA[i][paddingLength + j] = automatonData[i][j];
//             paddedCA[i][paddingLength + j] = 0;
//             // paddedCA[i][j] = automatonData[i][j];
//         }

//     }

//     return paddedCA;
// }

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
    int* rule = ruleToBinaryArray(ruleNumber);
    char** automatonData = runCellularAutomaton(rule, generations, initialConditions);

    int initialConditionsLength = strlen(initialConditions);
    printf("Initial conditions length: %d\n", initialConditionsLength);
    // char** paddedCA = padCellularAutomata(automatonData, generations, initialConditionsLength);

    int imageWidth = strlen(initialConditions) + 2 * generations;
    outputToFile(automatonData, ruleNumber, generations, initialConditions, imageWidth);
    // outputToFile(paddedCA, ruleNumber, generations, initialConditions, imageWidth);
    
    clock_t end = clock();
    double duration = ((double)(end - start)) / CLOCKS_PER_SEC * 1000;
    printf("Took %fms to generate %d generations of rule %d\n", duration, generations, ruleNumber);

    // free paddedCA and its contents
    // for (int i = 0; i < generations; i++) {
    //     free(paddedCA[i]);
    // }
    // free(paddedCA);

    free(rule);
    free(automatonData);


    printf("Done!\n");

    return 0;
}