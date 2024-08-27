#include <stdlib.h>
#include <stdio.h>
#include <string.h>

 void simple(int n, char* plain, char* enc_string, int step)
 {
    for (int i = n - 1; i >= 0; i--) {
        enc_string[i] = plain[i] + step;
        if (enc_string[i] > 'Z') {
            enc_string[i] = enc_string[i] - 'Z' - 1 + 'A';
        } 
    }
 }

int main()
{
    char *s1 = malloc(20 * sizeof(char));
    char *s2 = malloc(20 * sizeof(char));
    strcpy(s1, "ABRAHAM");
    strcpy(s2, "");
    simple(strlen(s1), s1, s2, 26);
    printf("%s", s2);
    return 0;
}