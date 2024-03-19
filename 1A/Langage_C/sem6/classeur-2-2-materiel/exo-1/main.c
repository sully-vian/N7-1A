#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "tab.h"

// Macros pour les options du programme
#define QUITTER 0
#define AJOUTER 1
#define SUPPRIMER 2

static const double SEUIL = 0.25;

void afficher_tab(const tab_t *tab) {
    printf("Tableau : [");
    for (int i = 0; i < taille(tab) - 1; i++) {
        printf("%d, ", element(tab, i));
    }
    if (taille(tab) == 0) {
        printf("]\n");
    } else {
        printf("%d]\n", element(tab, taille(tab) - 1));
    }
}

double taux(const tab_t *tab) {
    return (double)taille(tab) / (double)espace(tab);
}

void afficher_menu() {
    printf("0. Quitter\n");
    printf("1. Ajouter un élément\n");
    printf("2. Supprimer un élément\n");
}

void traiter_choix(tab_t *tab, int choix, int *quitter) {
    int elt;
    switch (choix) {
        case QUITTER:
            *quitter = false;
            break;

        case AJOUTER:
            printf("Elément à ajouter : ");
            if (scanf("%d", &elt) != 1) {
                printf("Erreur de saisie\n");
                break;
            }
            ajouter(tab, elt);
            break;

        case SUPPRIMER:
            if (taille(tab) == 0) {
                printf(
                    "Vous ne pouvez pas retirer d'élément d'un tableau "
                    "vide.\n");
                break;
            }
            printf("Elément à supprimer : ");
            if (scanf("%d", &elt) != 1) {
                printf("Erreur de saisie\n");
                break;
            }
            supprimer(tab, elt);

            if (taux(tab) < SEUIL) {
                printf("Le tableau est redimensionné !\n");
                serrer(tab);
            }
            break;

        default:
            printf("Le choix '%d' est invalide\n", choix);
            break;
    }
    while (getchar() != '\n')  // vider le buffer
        ;
}

int main() {
    tab_t *tab = creer();
    if (tab == NULL) {
        printf("Erreur : l'allocation en mémoire n'a pas pu se faire\n");
        return 1;
    }
    int choix;
    int quitter = false;

    do {
        afficher_tab(tab);
        printf("Taux d'occupation : %.2lf%%\n", taux(tab));
        afficher_menu();
        printf("Choix : ");
        if (scanf("%d", &choix) != 1) {
            printf("Option invalide\n");
            while (getchar() != '\n')  // vider le buffer
                ;
            continue;
        }
        traiter_choix(tab, choix, &quitter);

    } while (!quitter);

    printf("Fin du programme.");
    detruire(&tab);

    return 0;
}