// exercitiul 4: utilitarul 4
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#define BUFFER_SIZE 4096

void displayFileContent(int fileDescriptor) {
    char buffer[BUFFER_SIZE];
    ssize_t bytesRead;

    while ((bytesRead = read(fileDescriptor, buffer, sizeof(buffer))) > 0) {
        for (ssize_t i = 0; i < bytesRead; ++i) {
            putchar(buffer[i]);

            if (buffer[i] == '\n') {
                // Afișează linie cu linie
                fflush(stdout);
            }
        }
    }

    if (bytesRead == -1) {
        perror("Eroare la citirea fișierului");
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Nunmar invalid de argumente\n");
        exit(EXIT_FAILURE);
    }

    for (int i = 1; i < argc; ++i) {
        // Deschide fiecare fișier în mod citire
        int fileDescriptor = open(argv[i], O_RDONLY);

        if (fileDescriptor == -1) {
            perror("Eroare la deschiderea fișierului");
            // Continuă cu următorul fișier
            continue;
        }

        // Afișează conținutul fiecărui fișier
        displayFileContent(fileDescriptor);

        // Închide fișierul
        if (close(fileDescriptor) == -1) {
            perror("Eroare la închiderea fișierului");
            exit(EXIT_FAILURE);
        }
    }

    return 0;
}
