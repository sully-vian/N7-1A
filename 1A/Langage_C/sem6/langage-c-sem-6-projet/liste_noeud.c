#define _GNU_SOURCE
#include "liste_noeud.h"
#include <math.h>
#include <stdlib.h>

struct cellule_t {
    noeud_id_t noeud;
    noeud_id_t precedent;
    float distance;
};
typedef struct cellule_t cellule_t;

struct liste_noeud_t {
    cellule_t cellule;
    liste_noeud_t *suivant;
};

liste_noeud_t *creer_liste() {
    liste_noeud_t *liste = malloc(sizeof(liste_noeud_t));
    if (liste == NULL) {
        printf("Échec de l'allocation dynamique\n");
        return NULL;
    }
    liste->suivant = NULL;
    liste->cellule.noeud = NO_ID;
    liste->cellule.distance = INFINITY;
    liste->cellule.precedent = NO_ID;
    return liste;
}

void detruire_liste(liste_noeud_t **liste_ptr) {
    if (*liste_ptr == NULL) {
        return;
    } else {
        detruire_liste(&((*liste_ptr)->suivant));
        free(*liste_ptr);
        *liste_ptr = NULL;
    }
}

bool est_vide_liste(const liste_noeud_t *liste) {
    // printf("est vide ?\n");
    return (liste->cellule.noeud == NO_ID) && (liste->suivant == NULL);
}

bool contient_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (est_vide_liste(liste)) {
        return false;
    } else if (liste->cellule.noeud == noeud) {
        return true;
    } else {
        return contient_noeud_liste(liste->suivant, noeud);
    }
}

bool contient_arrete_liste(const liste_noeud_t *liste,
                           noeud_id_t source,
                           noeud_id_t destination) {
    if (est_vide_liste(liste)) {
        return false;
    } else if ((liste->cellule.noeud == destination) &&
               (liste->cellule.precedent == source)) {
        return true;
    } else {
        return contient_arrete_liste(liste->suivant, source, destination);
    }
}

float distance_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (est_vide_liste(liste)) {
        return INFINITY;
    } else if (liste->cellule.noeud == noeud) {
        return liste->cellule.distance;
    } else {
        return distance_noeud_liste(liste->suivant, noeud);
    }
}

noeud_id_t precedent_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (est_vide_liste(liste)) {
        return NO_ID;
    } else if (liste->cellule.noeud == noeud) {
        return liste->cellule.precedent;
    } else {
        return precedent_noeud_liste(liste->suivant, noeud);
    }
}

static cellule_t min_noeud_liste_aux(const liste_noeud_t *liste) {
    if (est_vide_liste(liste)) {
        return liste->cellule;
    } else {
        cellule_t cellule_min = min_noeud_liste_aux(liste->suivant);
        float distance = liste->cellule.distance;
        float distance_min = cellule_min.distance;
        return (distance < distance_min) ? liste->cellule
                                                                : cellule_min;
    }
}

noeud_id_t min_noeud_liste(const liste_noeud_t *liste) {
    cellule_t cellule = min_noeud_liste_aux(liste);
    return cellule.noeud;
}

void inserer_noeud_liste(liste_noeud_t *liste,
                         noeud_id_t noeud,
                         noeud_id_t precedent,
                         float distance) {
    // copie du premier terme de la liste fournie
    liste_noeud_t *nouv_liste = creer_liste();
    nouv_liste->cellule.noeud = liste->cellule.noeud;
    nouv_liste->cellule.distance = liste->cellule.distance;
    nouv_liste->cellule.precedent = liste->cellule.precedent;
    nouv_liste->suivant = liste->suivant;

    // remplacement des champs de la première cellule
    liste->cellule.noeud = noeud;
    liste->cellule.distance = distance;
    liste->cellule.precedent = precedent;
    liste->suivant = nouv_liste;
}

void changer_noeud_liste(liste_noeud_t *liste,
                         noeud_id_t noeud,
                         noeud_id_t precedent,
                         float distance) {
    if (est_vide_liste(liste)) {
        inserer_noeud_liste(liste, noeud, precedent, distance);
    } else if (liste->cellule.noeud == noeud) {
        liste->cellule.precedent = precedent;
        liste->cellule.distance = distance;
    } else {
        changer_noeud_liste(liste->suivant, noeud, precedent, distance);
    }
}

void supprimer_noeud_liste(liste_noeud_t *liste, noeud_id_t noeud) {
    if (est_vide_liste(liste)) {
        return;
    } else if (liste->cellule.noeud == noeud) {
        // copie du suivant dans l'actuel
        liste_noeud_t *suivant = liste->suivant;
        liste->cellule.noeud = suivant->cellule.noeud;
        liste->cellule.precedent = suivant->cellule.precedent;
        liste->cellule.distance = suivant->cellule.distance;
        liste->suivant = suivant->suivant;

        // supression du suivant
        free(suivant);
        suivant = NULL;
    } else {
        supprimer_noeud_liste(liste->suivant, noeud);
    }
}

// static void debug_cellule_t(cellule_t cellule) {
//     printf("[%lu, %lu, %.1lf]", cellule.noeud, cellule.precedent,
//            cellule.distance);
// }

static void debug_noeud(noeud_id_t noeud) {
    printf("[%lu]", noeud);
}

void debug_liste(const liste_noeud_t *liste) {
    if (est_vide_liste(liste)) {
        printf("-|\n");
    } else {
        printf("->");
        // debug_cellule_t(liste->cellule);
        debug_noeud(liste->cellule.noeud);
        debug_liste(liste->suivant);
    }
}