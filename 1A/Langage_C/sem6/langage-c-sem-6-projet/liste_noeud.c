#define _GNU_SOURCE
#include "liste_noeud.h"
#include <math.h>
#include <stdlib.h>

struct _cellule {
    noeud_id_t noeud;
    float distance;
    noeud_id_t precedent;
};
typedef struct _cellule _cellule;

struct liste_noeud_t {
    _cellule cellule;
    liste_noeud_t *suivant;
};

liste_noeud_t *creer_liste() {
    liste_noeud_t *liste;
    liste->suivant = NULL;
    liste->cellule.noeud = NO_ID;
    liste->cellule.distance = 0;
    liste->cellule.precedent = NO_ID;
    return liste;
}

void detruire_liste(liste_noeud_t **liste_ptr) {
    detruire_liste((*liste_ptr)->suivant);
    free(*liste_ptr);
    *liste_ptr = NULL;
}

bool est_vide_liste(const liste_noeud_t *liste) {
    return (liste->cellule.noeud == NO_ID) && (liste->suivant == NULL);
}

bool contient_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (liste == NULL) {
        return false;
    } else if (liste->cellule.noeud == noeud) {
        return true;
    } else {
        return contient_noeud_liste(liste->suivant, noeud);
    }
}

float distance_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (liste = NULL) {
        return INFINITY;
    } else if (liste->cellule.noeud == noeud) {
        return liste->cellule.distance;
    } else {
        return distance_noeud_liste(liste->suivant, noeud);
    }
}
