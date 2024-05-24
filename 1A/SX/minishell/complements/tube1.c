#include <features.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/* Ne peut pas marcher puisque le descripteur de fichier du fils ne contient
 * pas le tube créé par le père APRÉS le fork.
 * De plus, il faudrait que le fils attende la le descriteur de fichier.
 */

int main() {
    int p[2];
    switch(fork()) {
        case -1:
            fprintf(stderr, "Erreur au fork");
            exit(EXIT_FAILURE);
            break;

        case 0: /* fils */
                int alire;
                int nlus;
                if ((nlus = read(p[0], &alire, 1)) < 0) {
                    fprintf(stderr, "Erreur à la lecture du tube par le fils");
                    exit(EXIT_FAILURE);
                }
                fprintf(stdout, "Valeur lue : %d\n", alire);
                fprintf(stdout, "Nombre d'octets lus : %d\n", nlus);
            break;

        default: /* pere */
            if (pipe(p) == -1) {
                fprintf(stderr, "Erreur à la création du tube");
                exit(EXIT_FAILURE);
            }

            int aecrire = 3;
            if (write(p[1], &aecrire, 1) == -1) {
                fprintf(stderr, "Erreur à l'écriture dans le tube par le père");
                exit(EXIT_FAILURE);
            };
            break;
    }
}