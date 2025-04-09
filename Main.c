#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LENGTH_INITIAL_CONDITIONS 1000

char* ruleToBinaryArray(char ruleNumber) {
    char* ruleBinary = (char*)malloc(8 * sizeof(char));
    for (char i = 0; i < 8; i++) 
        ruleBinary[i] = (ruleNumber >> i) & 1;
    return ruleBinary;
}

// the neighborhood must be an actual string, with ascii values of 0 and 1 and a null terminator
char calculateCell(const char *neighborhood, const char* ruleBinary) {
    // Convert neighborhood directly to index using bit operations
    char index = ((neighborhood[0] - '0') << 2) | 
                 ((neighborhood[1] - '0') << 1) | 
                 (neighborhood[2] - '0');
    return ruleBinary[index];
}

char** runCellularAutomaton(const char* rule, const int generations, char* cells, const int initialConditionsLength) {
    const int imageWidth = initialConditionsLength + 2 * generations;
    // Allocate all memory at once
    char* memoryBlock = (char*)malloc(generations * (imageWidth + 1) * sizeof(char));
    if (NULL == memoryBlock) {
        printf("Error allocating memory!\n");
        return NULL;
    }

    char** automatonData = (char**)malloc(generations * sizeof(char*));
    if (NULL == automatonData) {
        printf("Error allocating memory!\n");
        free(memoryBlock);
        return NULL;
    }

    // Set up row pointers
    for (int i = 0; i < generations; i++) {
        automatonData[i] = memoryBlock + i * (imageWidth + 1);
        automatonData[i][imageWidth] = '\0';
    }

    int length = initialConditionsLength;
    int initialOffset = (imageWidth - initialConditionsLength) / 2;

    // Initialize first generation
    memset(automatonData[0], 0, imageWidth);
    memcpy(automatonData[0] + initialOffset, cells, initialConditionsLength);

    length += 2;

    // Use a static buffer for neighborhood
    static char neighborhood[4];
    neighborhood[3] = '\0';

    for (int i = 1; i < generations; i++) {
        int paddingOffset = initialOffset - i;
        char* currentRow = automatonData[i];
        char* previousRow = automatonData[i - 1];

        // Clear the current row
        memset(currentRow, 0, imageWidth);

        for (int j = paddingOffset; j < paddingOffset + length; j++) {
            neighborhood[0] = previousRow[j - 1] + '0';
            neighborhood[1] = previousRow[j] + '0';
            neighborhood[2] = previousRow[j + 1] + '0';

            currentRow[j] = calculateCell(neighborhood, rule);
        }

        length += 2;
    }

    return automatonData;
}

int outputToFile(char** automatonData, int ruleNumber, const int generations, const char *initialConditions, const int imageWidth) {
    char filename[MAX_LENGTH_INITIAL_CONDITIONS + 50];
    sprintf(filename, "results/r%d_g%d_i%s_c.pbm", ruleNumber, generations, initialConditions);
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        return 1;
    }

    fprintf(file, "P1\n%d %d\n", imageWidth, generations);
    for (int i = 0; i < generations; i++) {
        for (int j = 0; j < imageWidth; j++) 
            fprintf(file, "%d", automatonData[i][j]);
        
        fprintf(file, "\n");
    }
    return fclose(file);
}

int main() {
    int exitCode = 0;
    FILE *inputFile = fopen("input.txt", "r");
    if (!inputFile) {
        printf("Error opening input file!\n");
        return 1;
    }

    int resultFlag;
    int ruleNumber, generations;
    char initialConditions[MAX_LENGTH_INITIAL_CONDITIONS];
    resultFlag = fscanf(inputFile, "%d %s %d", &ruleNumber, initialConditions, &generations);
    if (3 != resultFlag) {  // example: if there are less than 3 lines in the input.txt 
        printf("Error reading input file! Got %d elements instead of the expectecd 3\n", resultFlag);
        fclose(inputFile);
        return 1;
    }

    resultFlag = fclose(inputFile);

    if (0 != resultFlag) {
        printf("Error closing input file!\n");
        return 1;
    }

    char* rule = ruleToBinaryArray(ruleNumber);
    int initialConditionsLength = strlen(initialConditions);

    char* cells = (char*)malloc((initialConditionsLength + 1) * sizeof(char));
    // char* cells = NULL; // UNCOMMENT TO TEST ERROR HANDLING .. REMEMBER TO COMMENT OUT THE LINE ABOVE 
    if (NULL == cells) {
        printf("Error allocating memory!\n");
        exitCode = 1;
        goto CLEAN_UP_AND_EXIT_3;
    }

    cells[initialConditionsLength] = '\0';

    for (int i = 0; i < initialConditionsLength; i++) 
        cells[i] = initialConditions[i] - '0';
    
    char** automatonData = runCellularAutomaton(rule, generations, cells, initialConditionsLength);
    // char** automatonData = NULL; // UNCOMMENT TO TEST ERROR HANDLING .. REMEMBER TO COMMENT OUT THE LINE ABOVE
    if (NULL == automatonData) {
        printf("Error running cellular automaton!\n");
        exitCode = 1;
        goto CLEAN_UP_AND_EXIT_2;
    }

    int imageWidth = strlen(initialConditions) + 2 * generations;
    resultFlag = outputToFile(automatonData, ruleNumber, generations, initialConditions, imageWidth);
    // resultFlag = 1; // UNCOMMENT TO TEST ERROR HANDLING
    if (0 != resultFlag) {
        printf("Error writing to file!\n");
        exitCode = 1;
        goto CLEAN_UP_AND_EXIT;
    }

CLEAN_UP_AND_EXIT:
    if (automatonData != NULL) {
        free(automatonData[0]); // Free the entire memory block
        free(automatonData);
    }

CLEAN_UP_AND_EXIT_2:
    free(cells);

CLEAN_UP_AND_EXIT_3:
    free(rule);

    return exitCode;
}