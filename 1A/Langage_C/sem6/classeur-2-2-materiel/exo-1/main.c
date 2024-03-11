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
    switch (choix) {
        case QUITTER:
            *quitter = 1;
            break;

        case AJOUTER:
            int elt;
            printf("Elément à ajouter : ");
            scanf("%d", &elt);
            ajouter(tab, elt);
            break;

        case SUPPRIMER:
            if (taille(tab) == 0) {
                printf(
                    "Vous ne pouvez pas retirer d'élément d'un tableau "
                    "vide.\n");
            } else {
                int elt;
                printf("Elément à supprimer : ");
                scanf("%d", &elt);
                supprimer(tab, elt);

                if (taux(tab) < SEUIL) {
                    printf("Le tableau est redimensionné !\n");
                    serrer(tab);
                }
            }
            break;

        default:
            printf("Le choix '%d' est invalide", choix);
            break;
    }
}

int main() {
    tab_t *tab = creer();
    int choix;
    int quitter = 0;

    do {
        afficher_tab(tab);
        printf("Taux d'occupation : %.2lf%%\n", taux(tab));
        afficher_menu();
        printf("Choix : ");
        scanf("%d", &choix);
        traiter_choix(tab, choix, &quitter);

    } while (quitter != 1);

    printf("Fin du programme.");
    detruire(&tab);

    return 0;
}