#define _GNU_SOURCE

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define N 10

void traitement(int sig) {
    sig = sig;
    // vide car on ignore
}

int main() {
    char tableau[N];
    char tampon[128];
    pid_t pid_fils;

    int p[2];
    if (pipe(p) == -1) {
        fprintf(stderr, "Erreur à la création du tube");
        exit(EXIT_FAILURE);
    }

    switch (pid_fils = fork()) {
        case -1:
            fprintf(stderr, "Erreur au fork");
            exit(EXIT_FAILURE);
            break;

        case 0: /* fils */
            NULL;

            struct sigaction action;
            action.sa_handler = traitement;
            sigemptyset(&action.sa_mask);
            action.sa_flags = 0;
            sigaction(SIGUSR1, &action, NULL);

            pause();

            int nlus;
            if ((nlus = read(p[0], tampon, 10 * N)) < 0) {
                fprintf(stderr, "Erreur de lecture du tube");
                exit(EXIT_FAILURE);
            }

            break;

        default: /* père */
            printf("pid du fils = %d\n", pid_fils);
            for (;;) {
                int necrits = write(p[1], tableau, N);
                sleep(1);
                fprintf(stdout, "necrits = %d\n", necrits);
            }

            break;
    }
}