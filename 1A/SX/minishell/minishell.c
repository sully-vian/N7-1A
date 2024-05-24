#define _GNU_SOURCE

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
                printf("sortie du processus de pid = %d\n", pid);
            }
            if (WIFSIGNALED(status)) {
                printf("terminaison du processus de pid = %d par le signal %d\n", pid, sig);
            }
            if (WIFSTOPPED(status)) {
                printf("interruption du processus de pid = %d\n", pid);
            }
            if (WIFCONTINUED(status)) {
                printf("reprise du processus de pid = %d\n", pid);
            }
            break;

        default:
            printf("autre signal\n");
            break;
    }
}

/* Ouvrir open en assurant la bonne exécution. Retourne le descripteur de fichier associé. */
int safeopen(char *nom, int flags, mode_t mode) {
    int desc_open;
    if (mode) {
        desc_open = open(nom, flags, mode);
    } else {
        desc_open = open(nom, flags);
    }
    if (desc_open == -1) {
        fprintf(stderr, "Erreur à l'ouverture de %s", nom);
        exit(EXIT_FAILURE);
    }
    return desc_open;
}

/* Fermer le fichier de descripteur `desc` en vérifiant la bonne fermeture. `nom` est utilisé en cas d'erreur. */
void safeclose(int desc, char *nom) {
    int desc_close = close(desc);
    if (desc_close == -1) {
        fprintf(stderr, "Erreur à la fermeture du descripteur %s", nom);
        exit(EXIT_FAILURE);
    }
}

/* Dupliquer (cf `dup2`) en assurant la bonne exécution. `nom` est utlisé en cas d'erreur. */
void safedup2(int oldfd, int newfd, char *nom) {
    if (dup2(oldfd, newfd) == -1) {
        fprintf(stderr, "Erreur au dup %s", nom);
        exit(EXIT_FAILURE);
    }
}

/* Executer `cmd0` avec `execvp` en s'assurant de la bonne exécution. */
void safeexecvp(char *cmd0, char *const argv[]) {
    if (execvp(cmd0, argv) == -1) {
        fprintf(stderr, "Commande inconnue : %s 😥", cmd0);
        exit(EXIT_FAILURE);
    }
}

int main(void) {
    bool fini = false;
    int dernier_lu = -1;

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
                            int tube[2];
                            if (commande->seq[indexseq + 1] != NULL) {
                                pipe(tube);
                            }

                            switch (pid_fils = fork()) {
                                case -1: /* fork fail */
                                    printf("Erreur lors du fork\n");
                                    break;

                                case 0: /* code fils */

                                    // changer groupe si en arrière plan
                                    if (commande->backgrounded != NULL) {
                                        setpgrp();
                                    }

                                    // remplacer l'entrée standard par commande->in
                                    char *in = commande->in;
                                    if (in != NULL) { /* cmd < file */
                                        int in_desc = safeopen(in, O_RDONLY, 0);
                                        safedup2(in_desc, 0, in);
                                        safeclose(in_desc, in);
                                    }

                                    // remplacer la sortie standard par commande->out
                                    char *out = commande->out;
                                    if (out != NULL) { /* cmd > file */
                                        int out_desc = safeopen(out, O_WRONLY | O_CREAT | O_TRUNC, 0644);
                                        safedup2(out_desc, 1, out);
                                        safeclose(out_desc, out);
                                    }

                                    // redirection des tubes
                                    if (dernier_lu != -1) {
                                        safedup2(dernier_lu, STDIN_FILENO, "dernier_lu");
                                        safeclose(dernier_lu, "dernier_lu1");
                                    }
                                    if (commande->seq[indexseq + 1] != NULL) {
                                        safeclose(tube[0], "dernier_lu2");
                                        safedup2(tube[1], STDOUT_FILENO, "tube[1]");
                                        safeclose(tube[1], "dernier_lu3");
                                    }

                                    // exécution de la commande
                                    safeexecvp(cmd[0], cmd);
                                    break;

                                default: /* code père */

                                    if (dernier_lu != -1) {
                                        safeclose(dernier_lu,"dernier_lu4");
                                    }
                                    if (commande->seq[indexseq + 1] != NULL) {
                                        safeclose(tube[1], "dernier_lu5");
                                        dernier_lu = tube[0];
                                    }

                                    // le père attend la terminaison du fils si ce dernier n'est pas en arrière plan
                                    if (commande->backgrounded == NULL) {
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
                if (dernier_lu != -1) {
                    // safeclose(dernier_lu, "dernier_lu6");
                }
                close(dernier_lu);
            }
        }
        // safeclose(dernier_lu, "dernier_lu7");
        close(dernier_lu);
        dernier_lu = -1;  // reset variable pour permettre la lecture de la prochaine commande tubée
    }
    return EXIT_SUCCESS;
}
