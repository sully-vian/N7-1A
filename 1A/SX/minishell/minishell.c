#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "readcmd.h"

int main(void) {
    bool fini = false;

    while (!fini) {
        printf("> ");
        struct cmdline *commande = readcmd();

        if (commande == NULL) {
            // commande == NULL -> erreur readcmd()
            perror("erreur lecture commande \n");
            exit(EXIT_FAILURE);

        } else {
            if (commande->err) {
                // commande->err != NULL -> commande->seq == NULL
                printf("erreur saisie de la commande : %s\n", commande->err);

            } else {
                /* Pour le moment le programme ne fait qu'afficher les commandes
                   tapees et les affiche à l'écran.
                   Cette partie est à modifier pour considérer l'exécution de
                   ces commandes
                */
                int indexseq = 0;
                char **cmd;
                while ((cmd = commande->seq[indexseq])) {
                    if (cmd[0]) {
                        if (strcmp(cmd[0], "exit") == 0) {
                            fini = true;
                            printf("Au revoir ...\n");
                        } else {
                            pid_t pid_fils;
                            int status;
                            switch (pid_fils = fork()) {
                                case -1: /* fork fail */
                                    printf("Erreur lors du fork\n");
                                    break;

                                case 0: /* code fils */
                                    if (execvp(cmd[0], cmd) == -1) {
                                        printf("Commande inconnue :-(\n");
                                        exit(EXIT_FAILURE);
                                    }
                                    break;

                                default: /* code père */
                                    if (commande->backgrounded == NULL) {
                                        wait(&status);
                                    }
                                    break;
                            }
                        }

                        indexseq++;
                    }
                }
            }
        }
    }
    return EXIT_SUCCESS;
}
