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
    char index = strtol(neighborhood, NULL, 2);
    return ruleBinary[index];
}

char** runCellularAutomaton(const char* rule, const int generations, char* cells, int initialConditionsLength) {
    const int imageWidth = initialConditionsLength + 2 * generations;
    char** automatonData = (char**)malloc(generations * sizeof(char*));
    if (NULL == automatonData) {
        printf("Error allocating memory!\n");
        return NULL;
    }

    int length = initialConditionsLength;
    int initialOffset = (imageWidth - initialConditionsLength) / 2;

    // allocate memory for the entire Cellular Automaton
    for (int i = 0; i < generations; i++) {
        char* row = (char*)malloc((imageWidth + 1)* sizeof(char));
        if (NULL == row) {
            printf("Error allocating memory!\n");
            // free the memory allocated by this function so far
            return NULL; // return null to indicate that the function failed and the caller should gracefully exit
        }
        memset(row, 0, (imageWidth + 1) * sizeof(char));
        row[imageWidth] = '\0';

        if (i == 0) 
            memcpy(row + initialOffset, cells, initialConditionsLength * sizeof(char));

        automatonData[i] = row;
    }

    length += 2;

    char neighborhood[4];
    neighborhood[3] = '\0'; 

    for (int i = 1; i < generations; i++) {
        int paddingOffset = initialOffset - i;

        for (int j = paddingOffset; j < paddingOffset + length; j++) {
            char* previousRow = automatonData[i - 1];

            neighborhood[0] = previousRow[j - 1] + '0';
            neighborhood[1] = previousRow[j] + '0';
            neighborhood[2] = previousRow[j + 1] + '0';

            automatonData[i][j] = calculateCell(neighborhood, rule);
        }

        length += 2; 
    }

    return automatonData;
}

int outputToFile(char** automatonData, int ruleNumber, int generations, const char *initialConditions, int imageWidth) {
    char filename[MAX_LENGTH_INITIAL_CONDITIONS + 50];
    sprintf(filename, "results/r%d_g%d_i%s_c.pbm", ruleNumber, generations, initialConditions);
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        return 1;
    }

    fprintf(file, "P1\n%d %d\n", imageWidth, generations);
    for (int i = 0; i < generations; i++) {
        int generationSize = strlen(automatonData[i]);
        for (int j = 0; j < imageWidth; j++) 
            fprintf(file, "%d", automatonData[i][j]);
        
        fprintf(file, "\n");
    }
    fclose(file);
    return 0; // success
}

int main() {
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
    if (NULL == cells) {
        printf("Error allocating memory!\n");
        free(rule);
        return 1;
    }

    cells[initialConditionsLength] = '\0';

    for (int i = 0; i < initialConditionsLength; i++) 
        cells[i] = initialConditions[i] - '0';
    
    char** automatonData = runCellularAutomaton(rule, generations, cells, initialConditionsLength);
    if (NULL == automatonData) {
        printf("Error running cellular automaton!\n");
        free(cells);
        free(rule);
        return 1;
    }

    int imageWidth = strlen(initialConditions) + 2 * generations;
    resultFlag = outputToFile(automatonData, ruleNumber, generations, initialConditions, imageWidth);
    
    printf("Result Flag: %d\n", resultFlag);

    if (0 != resultFlag) {
        printf("Error writing to file!\n");

        for (int i = 0; i < generations; i++)
            free(automatonData[i]);

        free(automatonData);
        free(rule);
        free(cells);

        return 1;
    }

    for (int i = 0; i < generations; i++)
        free(automatonData[i]);

    free(automatonData);
    free(rule);
    free(cells);

    return 0;
}