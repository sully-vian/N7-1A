#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define BUFSIZE 1024
/*
 * Équivalent à la commande cp
 */
int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <source> <destination>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    int fd_src = open(argv[1], O_RDONLY);
    if (fd_src == -1) {
        fprintf(stderr, "Erreur lors de l'ouverture de %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }

    int fd_dst = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (fd_dst == -1) {
        fprintf(stderr, "Erreur lors de l'ouverture de %s\n", argv[2]);
        exit(EXIT_FAILURE);
    }

    char tampon[BUFSIZE];
    ssize_t lus;
    while ((lus = read(fd_src, tampon, BUFSIZE)) > 0) {
        write(fd_dst, tampon, lus);
    }

    if (close(fd_src) == -1) {
        fprintf(stderr, "Erreur lors de la fermeture de %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }
    if (close(fd_dst) == -1) {
        fprintf(stderr, "Erreur lors de la fermeture de %s\n", argv[2]);
        exit(EXIT_FAILURE);
    }
}
