#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#define TAILLE_TAMPON 1024

int main(int argc, char **argv) {
    char tampon[TAILLE_TAMPON];
    size_t lus;
    
    if (argc == 1) {
        while (lus = read(STDIN_FILENO, tampon, TAILLE_TAMPON)) {
            write(STDOUT_FILENO, tampon, lus);
        }
    } else {
        for (int i = 1; i < argc; i++) {
            int desc = open(argv[i], O_RDONLY);
            if (desc == -1) {
                write(STDERR_FILENO, "Aucun fichier de ce type\n", 25);
                return EXIT_FAILURE;
            }
            while (lus = read(desc, tampon, TAILLE_TAMPON)) {
                write(STDOUT_FILENO, tampon, lus);
            }
        }
    }
    return EXIT_SUCCESS;
}
