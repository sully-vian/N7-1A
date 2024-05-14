#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <stdbool.h>

#define BUFSIZE 512

void traiter(char tampon, char cde, int nb) {
    int i;
    switch(cde) {
        case 'X':
            break;
        case 'Q':
            exit(1);
            break;
        case 'R':
            write(1, tampon, nb);
            break;
        case 'M':
            for(i=0; i<nb; i++) {
                tampon[i] = toupper(tampon[i]);
            }
            write(1, tampon, nb);
            break;
        case 'm':
            for(i=0; i<nb; i++) {
                tampon[i] = tolower(tampon[i]);
            }
            write(1, tampon, nb);
            break;
        default:
            printf("????\n");
    }
}

int main(int argc, char argv[][]) {
    int p[2];
    pid_t pid;
    int d, nlus;
    char buf[BUFSIZE];
    char commande = 'R' /* mode normal */
    if (argc != 2) {
        printf("utilisation : %s <fichier source>\n", argv[0]);
        exit(1);
    }
    
    if (pipe(p) == -1) {
        perror("pipe");
        exit(2);
    }
    
    pid = fork();
    if (pid == -1) {
        perror("fork");
        exit(3);
    }
    if (pid == 0) { /* fils */
        d = open(argv[1], O_RDONLY);
        if (d == -1) {
            fprintf(stderr, "Impossible d'ouvrir le fichier");
            perror(argv[1]);
            exit(4);
        }
        
        close(p[0]); /* pas besoin de lire dans p pour le fils */
        while (true) {
            while ((nlus = read(d, buf, BUFSIZE)) > 0) { /* lire de d */
                /* read peut lire moins que le nombre d'octets demandés, en
                 * particulier lorsque la fin du fichier est atteinte. */
                 write(p[1], buf, nlus); /* mettre dans le tube */
                 sleep(5);
            }
            sleep(5);
            printf("on recommence...\n");
            lseek(d, (off_t) 0, SEEK_SET); /* mise à 0 su curseur (lecture & écriture, c'est le même */
        }
        
    } else { /* père */
        close(p[1]); /* pas besoin d'écrire dans p pour le père */
        system("stty -ica&non min 1");
    }
}
