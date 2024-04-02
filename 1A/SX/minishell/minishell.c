#define _GNU_SOURCE

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

static pid_t pid = 0;
static pid_t pid_fils;
static pid_t pid_ded;
static int status;
static bool ded;  // processus avant est ded

void traitement(int sig) {
    switch (sig) {
        default:
            pid = waitpid(-1, &status, WNOHANG | WUNTRACED | WCONTINUED);
            if (pid == pid_ded) {
                ded = true;
                printf("processus terminé pour pid = %d !\n", pid_ded);
            }
            break;
    }
}

int main(void) {
    bool fini = false;

    struct sigaction action;
    action.sa_handler = traitement;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART;

    /* ajouter procédure de traitement dans cas de SIGCHLD */
    sigaction(SIGCHLD, &action, NULL);

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
                   tapées et les affiche à l'écran.
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
                                        /* père attend la terminaison du fils */

                                        pid_ded = pid_fils;
                                        ded = false;
                                        /* tq processus avant plan pas ded */
                                        while (!ded) {
                                            /* attendre de recevoir un signal.
                                             * Quand réception, le handler sera
                                             * appelé -> wait */
                                            pause();
                                        }
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
