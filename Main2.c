#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
    Main2.c
    This program is an optimized version of Main.c Where Main.c allocates a chunk of memory for each 
    line, the goal of this implementation is to allocate a single chunk of memory for the entire cellular
    automaton. This should hopefully reduce the run time of the program compaired to Main.c
*/

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

char* runCellularAutomaton(const char* rule, const int generations, char* cells, int initialConditionsLength) {
    const int imageWidth = initialConditionsLength + 2 * generations;

    const int fullAutomatonSize = generations * imageWidth ; // generations * size of last generation
    char* automatonData = (char*)malloc(fullAutomatonSize * sizeof(char));
    if (NULL == automatonData) {
        printf("Error allocating memory!\n");
        return NULL;
    }

    int length = initialConditionsLength;
    int initialOffset = (imageWidth - initialConditionsLength) / 2;

    // set the entire automaton to 0
    memset(automatonData, 0, fullAutomatonSize * sizeof(char));

    // set the first generation to be the content of cells
    memcpy(automatonData + initialOffset, cells, initialConditionsLength * sizeof(char));

    length += 2;

    char neighborhood[4];
    neighborhood[3] = '\0'; 

    for (int i = 1; i < generations; i++) {
        int paddingOffset = initialOffset - i;

        for (int j = paddingOffset; j < paddingOffset + length; j++) {
            neighborhood[0] = automatonData[(i - 1) * imageWidth + j - 1] + '0';
            neighborhood[1] = automatonData[(i - 1) * imageWidth + j] + '0';
            neighborhood[2] = automatonData[(i - 1) * imageWidth + j + 1] + '0';
           
            automatonData[i * imageWidth + j] = calculateCell(neighborhood, rule);
        }

        length += 2; 
    }

    return automatonData;
}

int outputToFile(char* automatonData, int ruleNumber, int generations, const char *initialConditions, int imageWidth) {
    char filename[MAX_LENGTH_INITIAL_CONDITIONS + 50];
    sprintf(filename, "results/r%d_g%d_i%s_c2.pbm", ruleNumber, generations, initialConditions);
    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        return 1;
    }

    fprintf(file, "P1\n%d %d\n", imageWidth, generations);
    for (int i = 0; i < generations; i++) {
        for (int j = 0; j < imageWidth; j++) 
            fprintf(file, "%d", automatonData[i * imageWidth + j]);
        
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
    
    char* automatonData = runCellularAutomaton(rule, generations, cells, initialConditionsLength);
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
    // for (int i = 0; i < generations; i++)
    //     free(automatonData[i]);

    free(automatonData);

CLEAN_UP_AND_EXIT_2:
    free(cells);

CLEAN_UP_AND_EXIT_3:
    free(rule);

    return exitCode;
}