#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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
            while ((nblus = read(p[0], &lu, sizeof(lu))) > 0) {
                fprintf(stdout, "lu: %d, nblus: %d\n", lu, nblus);
            }

            // n'arrive jamais puisque read est bloquant
            fprintf(stdout, "Sortie de boucle\n");
            break;

        default: /* père */
            for (int i = 0; i < 10; i++) {
                if (write(p[1], &i, sizeof(i)) == -1) {
                    fprintf(stderr,
                            "Erreur à l'écriture dans le tube par le père");
                    exit(EXIT_FAILURE);
                }
            }
            pause();  // attendre un signal
            break;
    }
}
