#include <stdio.h>   // printf
#include <stdlib.h>  // exit
#include <unistd.h>  // fork, getpid, getppid

int main(int argc, char *argv[]) {
    int tempsPere, tempsFils;
    pid_t pid_fork;
    int variable = 0;

    tempsPere = 120;
    tempsFils = 60;

    pid_fork = fork();
    // bonne pratique : tester systématiquement le retour des primitives
    if (pid_fork == -1) {
        printf("Erreur fork\n");
        exit(1);
        /* par convention, renvoyer une valeur > 0 en cas d'erreur,
         * différente pour chaque cause d'erreur, ici 1 = erreur fork
         */
    }
    if (pid_fork == 0) { /* fils */
        printf("fils: processus %d, de père %d et code du fork %d\n", getpid(),
               getppid(), pid_fork);
        printf("fils: variable = %d\n", variable);
        sleep(tempsFils);
        printf("fin du fils, variable = %d\n", variable);
        exit(EXIT_SUCCESS);
        /* 	bonne pratique :
                terminer les processus par un exit explicite */
    } else { /* père */
        printf("père: processus %d, de père %d et code du fork %d\n", getpid(),
               getppid(), pid_fork);
        printf("pere: variable = %d\n", variable);
        sleep(tempsPere);
        printf("fin du père, variable = d\n");
    }
    return EXIT_SUCCESS; /* -> exit(EXIT_SUCCESS); pour le père */
}
