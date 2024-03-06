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
    return ruleBinary[index] + '0';
}

char* runCellularAutomaton(const char* rule, const int generations, char* cells, int initialConditionsLength) {
    const int imageWidth = initialConditionsLength + 2 * generations;

    const int fullAutomatonSize = generations * (imageWidth + 1) ; // generations * size of last generation
    char* automatonData = (char*)malloc((fullAutomatonSize + 1) * sizeof(char)); // plus 1 to hold the null terminator
    if (NULL == automatonData) {
        printf("Error allocating memory!\n");
        return NULL;
    }

    int length = initialConditionsLength;
    int initialOffset = (imageWidth - initialConditionsLength) / 2;

    // set the entire automaton to 0
    // memset(automatonData, 0, fullAutomatonSize * sizeof(char));
    memset(automatonData, '0', fullAutomatonSize * sizeof(char));

    // set the null terminator for each generation
    for (int i = 0; i < generations; i++) 
        // automatonData[i * imageWidth + imageWidth] = '\0';
        automatonData[i * imageWidth + imageWidth] = '\n';

    // set the very last char to be the null terminator
    automatonData[fullAutomatonSize] = '\0';

    // set the first generation to be the content of cells
    memcpy(automatonData + initialOffset, cells, initialConditionsLength * sizeof(char));

    length += 2;

    char neighborhood[4];
    neighborhood[3] = '\0'; 

    for (int i = 1; i < generations; i++) {
        int paddingOffset = initialOffset - i;

        for (int j = paddingOffset; j < paddingOffset + length; j++) {
            // neighborhood[0] = automatonData[(i - 1) * (imageWidth + 1) + j - 1] + '0';
            // neighborhood[1] = automatonData[(i - 1) * (imageWidth + 1)  + j] + '0';
            // neighborhood[2] = automatonData[(i - 1) * (imageWidth + 1) + j + 1] + '0';
             neighborhood[0] = automatonData[(i - 1) * (imageWidth + 1) + j - 1];
            neighborhood[1] = automatonData[(i - 1) * (imageWidth + 1)  + j] ;
            neighborhood[2] = automatonData[(i - 1) * (imageWidth + 1) + j + 1];
           
            automatonData[i * (imageWidth + 1) + j] = calculateCell(neighborhood, rule);
        }

        length += 2; 
    }

    return automatonData;
}

int outputToFile(char* automatonData, int ruleNumber, int generations, const char *initialConditions, int imageWidth) {
    char filename[MAX_LENGTH_INITIAL_CONDITIONS + 50];
    sprintf(filename, "results/r%d_g%d_i%s_c2.pbm", ruleNumber, generations, initialConditions);

    // Calculate the maximum possible size of the content
    // Each cell will be '0' or '1' plus a newline character at the end of each generation.
    // Plus the header size (assuming 20 characters for "P1\n", imageWidth, and generations)
    int maxSize = generations * (imageWidth + 1) + 20;
    char *content = (char *)malloc(sizeof(char) * maxSize);
    if (!content) {
        printf("Error allocating memory for file content!\n");
        return 1; // Memory allocation failed
    }

    // Start building the content
    int offset = sprintf(content, "P1\n%d %d\n", imageWidth, generations);
    
    for (int i = 0; i < generations; i++) {
        for (int j = 0; j < imageWidth; j++) 
            offset += sprintf(content + offset, "%d", automatonData[i * imageWidth + j]);
        
        offset += sprintf(content + offset, "\n");
    }

    FILE *file = fopen(filename, "w");
    if (!file) {
        printf("Error creating output file!\n");
        free(content); // Free allocated memory
        return 1;
    }

    // Write the entire content in one go
    fputs(content, file);
    fclose(file);
    
    // Free the allocated memory for the content
    free(content);

    return 0;
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
        // cells[i] = initialConditions[i] - '0';
        cells[i] = initialConditions[i];
    
    char* automatonData = runCellularAutomaton(rule, generations, cells, initialConditionsLength);
    // char** automatonData = NULL; // UNCOMMENT TO TEST ERROR HANDLING .. REMEMBER TO COMMENT OUT THE LINE ABOVE
    if (NULL == automatonData) {
        printf("Error running cellular automaton!\n");
        exitCode = 1;
        goto CLEAN_UP_AND_EXIT_2;
    }

    printf("automata data --> \"\"\"\n%s\n\"\"\"", automatonData); // UNCOMMENT TO PRINT THE AUTOMATON DATA (FOR TESTING PURPOSES

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