#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#define MAX_LINE_LENGTH 500
#define MAX_LINES 1000

char extractFirstDigit(const char *row);
char extractLastDigit(const char *row);
void readLines(const char *fileName, char lines[MAX_LINES][MAX_LINE_LENGTH]);

int main() {
    char lines[MAX_LINES][MAX_LINE_LENGTH];
    readLines("input.txt", lines);

    int sum = 0;
    
    for (int i = 0; i < sizeof(lines) / sizeof(lines[0]); ++i) {
        char firstDigit = extractFirstDigit(lines[i]);
        char lastDigit = extractLastDigit(lines[i]);

        if (lastDigit == 'O') {
            lastDigit = firstDigit;
        } 

        char result[3];  
        result[0] = firstDigit;
        result[1] = lastDigit;
        result[2] = '\0';  

        int num = atoi(result);
        sum = sum + num;
    }

    printf("%i\n", sum);

    return 0;
}


char extractFirstDigit(const char *row) {
    for (int i = 0; row[i] != '\0'; ++i) {
        if (isdigit(row[i])) {
            return row[i];
        }
    }
    return 'O';
} 

char extractLastDigit(const char *row) {
    int lastIndex = strlen(row) - 1;

    for (; lastIndex >= 0; --lastIndex) {
        if (isdigit(row[lastIndex])) {
            return row[lastIndex]; 
        }
    }
    return 'O'; 
}

void readLines(const char *fileName, char lines[MAX_LINES][MAX_LINE_LENGTH]) {
    FILE *file = fopen("input.txt", "r");

    if (file == NULL) {
        perror("Error opening file");
        return;
    }

    int lineCount = 0;
    while (lineCount < MAX_LINES && fgets(lines[lineCount], MAX_LINE_LENGTH, file) != NULL) {
        lineCount++;
    }

    fclose(file);
}