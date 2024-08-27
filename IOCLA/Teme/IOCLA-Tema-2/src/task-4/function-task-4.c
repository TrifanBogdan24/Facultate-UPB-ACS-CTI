#include <stdlib.h>
#include <stdio.h>
#include <string.h>

    // initializam tabla cu '0'
    // tabla este reprezentata printr-o matrice, liniile fiind privite de sus in jos
    // prima linie de pe tabla -> ultima linie dintr-o matrice C
void init_table(char table[8][8])
{
    for (int i  = 0; i < 8; i++)
        for (int j = 0; j < 8; j++)
            table[i][j] = '0';
}

void checkers(int x, int y, char table[8][8])
{
    if (x > 0 && y > 0)
        table[x - 1][y - 1] = '1';
    
    if (x > 0 && y < 7)
        table[x - 1][y + 1] = '1';
    
    if (x < 7 && y > 0)
        table[x + 1][y - 1] = '1';

    if (x < 7 && y < 7)
        table[x + 1][y + 1] = '1';
}

void display_table(char table[8][8])
{
    for (int i = 0; i < 8; i++) {
        for (int j = 0; j < 8; j++) {
            printf("%c", table[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

void vecinii_piesei(int x, int y, char table[8][8])
{
    if (x > 0) {
        
        if (y > 0)
            table[x - 1][y -1] = '1';   // vecinul stanga-sus

        if (y < 7)
            table[x - 1][y + 1] = '1'; // vecinul dreapta-sus

    }

    if (x < 7) {
        
        if (y > 0)
            table[x + 1][y -1] = '1';   // vecinul stanga-jos

        if (y < 7)
            table[x + 1][y + 1] = '1'; // vecinul dreapta-jos

    }
}

int main()
{
    char table[8][8];
    checkers(0, 0, table);
    for (int x = 0; x < 8; x++) {
        for (int y = 0; y < 8; y++) {
            printf("Dama pe pozitia %d %d:\n", x, y);
            init_table(table);
            checkers(x, y, table);
            display_table(table);
        }
    }
    return 0;
}