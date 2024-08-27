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

void vecinii_piesei(int x, int y, int board[2])
{
    int poz = 0;
    if (x > 0) {
        if (y > 0) {
            // vecinul stanga-sus : (x - 1, y - 1)
            poz = 0;
            if (x >= 2)
                poz = (x - 2) * 8;
            poz += y - 1;
            if (poz <= 32) {
                board[0] = board[0] | (0xFF << (poz * 8));
            } else {
                poz = poz - 32;
                board[1] = board[1] | (0xFF << (poz * 8));
            }
        }

        if (y < 7) {
            // vecinul dreapta-sus : (x - 1, y + 1)
            poz = 0;
            if (x >= 2)
                poz = (x - 2) * 8;
            poz += y + 1;
            if (poz <= 32) {
                board[0] = board[0] | (0xFF << (poz * 8));
            } else {
                poz = poz - 32;
                board[1] = board[1] | (0xFF << (poz * 8));
            }
        }

    }

    if (x < 7) {

        if (y > 0) {
            // vecinul stanga-jos : (x + 1, y - 1)
            poz = x * 8 + (y - 1);
            if (x <= 3) {
                board[0] = board[0] | (0xFF << (poz * 8));
            } else {
                poz = poz - 4 * 8;
                board[1] = board[1] | (0xFF << (poz * 8));
            }
        }

        if (y < 7) {
            // vecinul stanga-jos : (x + 1, y + 1)
            poz = x * 8 + (y + 1);
            if (x <= 3) {
                board[0] = board[0] | (0xFF << (poz * 8));
            } else {
                poz = poz - 4 * 8;
                board[1] = board[1] | (0xFF << (poz * 8));
            }
        }

    }
}

int main()
{
    int x = 0, y = 0, board[2] = {0, 0};
    vecinii_piesei(x, y, board);
    return 0;
}