#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

    int length = initialConditionsLength;
    int initialOffset = (imageWidth - initialConditionsLength) / 2;

    // allocate memory for the entire Cellular Automaton
    for (int i = 0; i < generations; i++) {
        char* row = (char*)malloc((imageWidth + 1)* sizeof(char));
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
            neighborhood[0] = automatonData[i - 1][j - 1] + '0';
            neighborhood[1] = automatonData[i - 1][j] + '0';
            neighborhood[2] = automatonData[i - 1][j + 1] + '0';

            automatonData[i][j] = calculateCell(neighborhood, rule);
        }

        length += 2; 
    }

    return automatonData;
}

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
        for (int j = 0; j < imageWidth; j++) 
            fprintf(file, "%d", automatonData[i][j]);
        
        fprintf(file, "\n");
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

    char* rule = ruleToBinaryArray(ruleNumber);
    int initialConditionsLength = strlen(initialConditions);

    char* cells = (char*)malloc((initialConditionsLength + 1) * sizeof(char));
    cells[initialConditionsLength] = '\0';

    for (int i = 0; i < initialConditionsLength; i++) {
        cells[i] = initialConditions[i] - '0';
    }
    
    char** automatonData = runCellularAutomaton(rule, generations, cells, initialConditionsLength);

    int imageWidth = strlen(initialConditions) + 2 * generations;
    outputToFile(automatonData, ruleNumber, generations, initialConditions, imageWidth);
    
    for (int i = 0; i < generations; i++)
        free(automatonData[i]);

    free(automatonData);
    free(rule);
    free(cells);

    return 0;
}