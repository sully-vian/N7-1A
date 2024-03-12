#include "tab.h"
#include <stdlib.h>

// Constante pour la taille initiale
const int TAILLE_INIT = 4;

/**
 * Type enregistrement tab_t, avec pour champs
 *  - un champ pour le contenu du tableau (tableau d'entiers dans le tas)
 *  - taille allouée du tableau
 *  - taille utilisée du tableau
 */
struct tab_t {
    int *contenu;
    int taille;
    int espace;
};

/**
 * creer - crée un tab_t avec une taille allouée initiale de 4 et une taille
 * utilisée de 0. Le contenu du tab_t est un tableau alloué dans le tas (de
 * taille 4 initialement)
 *
 * Post-condition :
 *   - le champ contenu du tab_t est non-NULL et est dans le tas
 *   - taille(resultat) == 0
 *   - espace(resultat) == 4
 *
 * @return tab_t nouvellement créé
 */
tab_t *creer() {
    tab_t *tab = (tab_t *)malloc(sizeof(tab_t));
    tab->contenu = malloc(TAILLE_INIT * sizeof(int));
    tab->taille = 0;
    tab->espace = TAILLE_INIT;
    return tab;
}

/**
 * detruire - détruit le tab_t, c'est-à-dire libère son contenu alloué dans le
 * tas.
 *
 * Pré-conditions : tab != NULL, contenu de tab valide (non NULL et pas déjà
 * libéré) Post-conditions : contenu de tab == NULL, mémoire libérée
 *
 * @param tab [in,out] tab_t à détruire
 */
void detruire(tab_t **tab) {
    free((*tab)->contenu);
    (*tab)->contenu = NULL;
    free(*tab);
    *tab = NULL;
}

/**
 * ajouter - ajoute un élément à la fin du tab_t, en réallouant le contenu si
 * besoin
 *
 * Pré-conditions : tab != NULL, tab non détruit
 * Post-conditions :
 *   - element(tab, taille(tab) - 1) == elt
 *   - taille(tab) == \old(taille(tab)) + 1
 *   - si \old(taille(tab)) == \old(espace(tab)), alors espace(tab) == 2 *
 * espace(tab)
 *
 * @param tab [in,out] tab_t dans lequel ajouter l'élément
 * @param elt élément à rajouter
 */
void ajouter(tab_t *tab, int elt) {
    if (tab->taille == tab->espace) {
        tab->espace *= 2;
        tab->contenu = realloc(tab->contenu, tab->espace * sizeof(int));
    }
    tab->taille++;
    tab->contenu[tab->taille - 1] = elt;
}

/**
 * supprimer - supprimer la première occurence d'un élément s'il existe, ou
 * laisse le tableau inchangé si l'élément n'existe pas.
 *
 * Pré-conditions : tab != NULL, tab non détruit
 * Post-conditions :
 *   - si elt n'appartient pas à tab, taille(tab) == \old(taille(tab))
 *   - sinon, taille(tab) == \old(taille(tab)) - 1 et nombre d'occurrences de
 * elt dans tab réduit de 1
 *   - espace(tab) == \old(espace(tab))
 *
 * @param tab [in,out] tab_t duquel supprimer l'élément
 * @param elt élément à supprimer
 */
void supprimer(tab_t *tab, int elt) {
    int i = 0;
    while ((tab->contenu[i] != elt) && (i < tab->taille)) {
        i++;
    }
    if (i < tab->taille) {
        tab->taille--;
    }
    while (i < tab->taille) {
        tab->contenu[i] = tab->contenu[i + 1];
        i++;
    }
}

/**
 * element - récupère l'ième élément dans le tableau.
 *
 * Pré-conditions : tab non détruit, 0 <= id < tab.taille_utilisee
 *
 * @param tab [in] tab dans lequel chercher l'élément
 * @param id indice à récupérer
 */
int element(const tab_t *tab, int id) {
    return tab->contenu[id];
}

/**
 * taille - récupère la taille (utilisée) du tableau.
 *
 * Pré-conditions : tab non détruit
 * Post-conditions : resultat >= 0
 *
 * @return taille du tableau (nombre d'éléments)
 */
int taille(const tab_t *tab) {
    return tab->taille;
}

/**
 * espace - récupère la taille (en mémoire) du tableau.
 *
 * Pré-conditions : tab non détruit
 * Post-conditions : resultat > 0
 *
 * @return espace occupé par le tableau
 */
int espace(const tab_t *tab) {
    return tab->espace;
}

/**
 * serrer - réalloue le tableau de façon à ce que la taille allouée soit égale à
 * la taille utilisée.
 *
 * Pré-conditions : tab != NULL, tab non-détruit
 *  => /!\ taille(tab) peut être égale à 0
 * Post-conditions :
 *   - si taille(tab) > 0 alors taille(tab) == espace(tab)
 *   - sinon, espace(tab) == \old(espace(tab))
 *
 * @param tab [in,out] tab_t à réallouer
 */
void serrer(tab_t *tab) {
    if (tab->taille > 0) {
        tab->espace = tab->taille;
        tab->contenu = realloc(tab->contenu, tab->espace * sizeof(int));
    }
}
