#define _GNU_SOURCE
#include "liste_noeud.h"
#include <math.h>
#include <stdlib.h>

struct _cellule {
    noeud_id_t noeud;
    noeud_id_t precedent;
    float distance;
};
typedef struct _cellule _cellule;

struct liste_noeud_t {
    _cellule cellule;
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
    if (liste == NULL) {
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
    if (liste == NULL) {
        return false;
    } else if ((liste->cellule.noeud == destination) &&
               (liste->cellule.precedent == source)) {
        return true;
    } else {
        return contient_arrete_liste(liste->suivant, source, destination);
    }
}

float distance_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (liste == NULL) {
        return INFINITY;
    } else if (liste->cellule.noeud == noeud) {
        return liste->cellule.distance;
    } else {
        return distance_noeud_liste(liste->suivant, noeud);
    }
}

noeud_id_t precedent_noeud_liste(const liste_noeud_t *liste, noeud_id_t noeud) {
    if (liste == NULL) {
        return NO_ID;
    } else if (liste->cellule.noeud == noeud) {
        return liste->cellule.precedent;
    } else {
        return precedent_noeud_liste(liste->suivant, noeud);
    }
}

noeud_id_t min_noeud_liste_aux(const liste_noeud_t *liste, float *min_ptr) {
    if (est_vide_liste(liste->suivant)) {
        *min_ptr = liste->cellule.distance;
        return liste->cellule.noeud;
    } else {
        float min1;
        noeud_id_t noeud1 = min_noeud_liste_aux(liste->suivant, &min1);
        return (min1 < *min_ptr) ? noeud1 : liste->cellule.noeud;
    }
}

noeud_id_t min_noeud_liste(const liste_noeud_t *liste) {
    float inf = INFINITY;
    return min_noeud_liste_aux(liste, &inf);
}

void inserer_noeud_liste(liste_noeud_t *liste,
                         noeud_id_t noeud,
                         noeud_id_t precedent,
                         float distance) {
    // printf("insérer: début - ");

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
    // printf("fin\n");
}

void changer_noeud_liste(liste_noeud_t *liste,
                         noeud_id_t noeud,
                         noeud_id_t precedent,
                         float distance) {
    if (liste == NULL) {
        inserer_noeud_liste(liste, noeud, precedent, distance);
    } else if (liste->cellule.noeud == noeud) {
        liste->cellule.precedent = precedent;
        liste->cellule.distance = distance;
    } else {
        changer_noeud_liste(liste->suivant, noeud, precedent, distance);
    }
}

void supprimer_noeud_liste(liste_noeud_t *liste, noeud_id_t noeud) {
    if (liste == NULL) {
        return;
    } else if (liste->cellule.noeud == noeud) {
        liste->cellule.noeud = NO_ID;
    } else {
        supprimer_noeud_liste(liste->suivant, noeud);
    }
}

void debug_cellule(_cellule cellule) {
    printf("[%lu, %lu, %.1lf]", cellule.noeud, cellule.precedent,
           cellule.distance);
}

void debug_liste(const liste_noeud_t *liste) {
    if (liste == NULL) {
        printf("-|\n");
    } else {
        printf("->");
        debug_cellule(liste->cellule);
        debug_liste(liste->suivant);
    }
}