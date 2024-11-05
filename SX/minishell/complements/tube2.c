#include <fcntl.h>
#include <features.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// - Un processus père crée un tube, puis écrit un entier dans le tube.
// – Il crée ensuite un fils qui lit un entier dans le tube et affiche l’entier lu.

int main() {
    int p[2];
    if (pipe(p) == -1) {
        fprintf(stderr, "Erreur à la création du tube");
        exit(EXIT_FAILURE);
    }

    int aecrire = 10;
    if (write(p[1], &aecrire, sizeof(aecrire)) == -1) {
        fprintf(stderr, "Erreur à l'écriture dans le tube par le père");
        exit(EXIT_FAILURE);
    }

    switch (fork()) {
        case -1:
            fprintf(stderr, "Erreur au fork");
            exit(EXIT_FAILURE);
            break;

        case 0: /* fils */
            int lu;
            if (read(p[0], &lu, sizeof(lu)) == -1) {
                fprintf(stderr, "Erreur à la lecture dans le tube par le fils");
                exit(EXIT_FAILURE);
            }
            fprintf(stdout, "Entier lu: %d\n", lu);

            break;

        default: /* père */
            break;
    }
}