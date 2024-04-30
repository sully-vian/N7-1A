#define _GNU_SOURCE

#include <features.h>
#include <fcntl.h>
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
static int status;
static const char *prompt = "> ";

/**
 * traitement du signal SIGCHLD
 */
void traitement(int sig) {

    switch (sig) {

        case SIGINT:
            // printf("\n[SIGINT]\n");
            break;

        case SIGTSTP:
            // printf("\n[SIGSTP]\n");
            break;

        case SIGCHLD:
            pid = waitpid(-1, &status, WNOHANG | WUNTRACED | WCONTINUED);

            if (WIFEXITED(status)) {
                printf("\nsortie du processus de pid = %d\n", pid);
            }
            if (WIFSIGNALED(status)) {
                printf("\nterminaison du processus de pid = %d par le signal %d\n", pid, sig);
            }
            if (WIFSTOPPED(status)) {
                printf("\ninterruption du processus de pid = %d\n", pid);
            }
            if (WIFCONTINUED(status)) {
                printf("\nreprise du processus de pid = %d\n", pid);
            }
            break;

        default:
            printf("autre signal\n");
            break;
    }
}

int main(void) {
    bool fini = false;

    struct sigaction action;
    action.sa_handler = traitement;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART;

    /* masquage de SIGINT et SIGTSTP en plus de ceux déjà masqués */
    // sigset_t mask;
    // sigemptyset(&mask);
    // sigaddset(&mask, SIGINT);
    // sigaddset(&mask, SIGTSTP);
    // sigprocmask(SIG_BLOCK, &mask, NULL);

    /* ajouter des procédures de traitement à la réception de signaux */
    sigaction(SIGCHLD, &action, NULL);
    sigaction(SIGINT, &action, NULL);
    sigaction(SIGTSTP, &action, NULL);

    while (!fini) {
        printf(prompt);
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
                                    if (commande->backgrounded != NULL) {
                                        /* changer groupe si en arrière plan */
                                        setpgrp();
                                    }
                                    
                                    /* replacer l'entrée standard par commande->in */
                                    char *in = commande->in;
                                    if (in != NULL) { /* cmd < f */
                                        int in_desc;
                                        if ((in_desc = open(in, O_RDONLY)) == -1) {
                                            fprintf(stderr, "Erreur à l'ouverture de %s", in);
                                            exit(EXIT_FAILURE);
                                        }
                                        if (dup2(in_desc, 0) == -1) {
                                            fprintf(stderr, "Erreur au dup in");
                                            exit(EXIT_FAILURE);
                                        }
                                        if (close(in_desc) == -1) {
                                            fprintf(stderr, "Erreur à la fermeture du descripteur in");
                                            exit(EXIT_FAILURE);
                                        }
                                    }
                                    
                                    /* replacer la sortie standard par commande->out */
                                    char *out = commande->out;
                                    if (out != NULL) { /* cmd > f */
                                        int out_desc;
                                        if ((out_desc = open(out, O_WRONLY|O_CREAT|O_TRUNC, 0644)) == -1) {
                                            fprintf(stderr, "Erreur à l'ouverture de %s", out);
                                            exit(EXIT_FAILURE);
                                        }
                                        if (dup2(out_desc, 1) == -1) {
                                            fprintf(stderr, "Erreur au dup out");
                                            exit(EXIT_FAILURE);
                                        }
                                        if (close(out_desc) == -1) {
                                            fprintf(stderr, "Erreur à la fermeture du descripteur out");
                                            exit(EXIT_FAILURE);
                                        }
                                    }
                                    
                                    if (execvp(cmd[0], cmd) == -1) { /* commande inconnue */
                                        printf("Commande inconnue :-(\n");
                                        exit(EXIT_FAILURE);
                                    }
                                    break;

                                default: /* code père */
                                    if (commande->backgrounded == NULL) {
                                        /* père attend la terminaison du fils */

                                            /* attendre de recevoir un signal.
                                             * Quand réception, le handler sera
                                             * appelé -> wait */
                                            pause();
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
