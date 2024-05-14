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
#include "readcmd.h"

// - Un processus père crée un tube, puis un fils.

// – Le père écrit une série d’entiers dans le tube, puis attend (par appel à
// pause()), puis se termine.

// – Le fils effectue une boucle consistant à lire un entier du tube, afficher
// l’entier et la valeur retournée par read(...), jusqu’à ce que cette valeur
// soit <= 0. Une fois sorti de la boucle, le fils affiche un message "sortie de
// boucle", puis se termine

int main() {
    int p[2];
    if (pipe(p) == -1) {
        fprintf(stderr, "Erreur à la création du tube");
        exit(EXIT_FAILURE);
    }

    switch (fork()) {
        case -1:
            fprintf(stderr, "Erreur au fork");
            exit(EXIT_FAILURE);
            break;

        case 0: /* fils */
            NULL;
            int lu;
            int nblus;
            while((nblus = read(p[0], &lu, sizeof(lu))) > 0) {
                fprintf(stdout, "lu: %d, nblus: %d\n", lu, nblus);
            }

            fprintf(stdout, "Sortie de boucle\n"); // n'arrive jamais puisque read est bloquant
            break;

        default: /* père */
            for (int i = 0; i < 10; i++) {
                if (write(p[1], &i, sizeof(i)) == -1) {
                    fprintf(stderr, "Erreur à l'écriture dans le tube par le père");
                    exit(EXIT_FAILURE);
                }
            }
            pause(); // attendre un signal
            break;
    }
}

// attendre que le père écrive dans le avant de lire
// if (fcntl(p[0], F_SETFL, fcntl(p[0], F_GETFL) | O_NONBLOCK) == -1) {
//     fprintf(stderr, "Erreur à la mise en mode non bloquant du tube par le
//     fils"); exit(EXIT_FAILURE);
// };