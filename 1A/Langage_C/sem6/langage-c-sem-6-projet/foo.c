#include "liste_noeud.h"

int main() {
    liste_noeud_t *t = creer_liste();
    inserer_noeud_liste(t, 1, 6, 1.0);
    inserer_noeud_liste(t, 2, 7, 1.5);
    inserer_noeud_liste(t, 3, 8, -0.1);
    debug_liste(t);
    noeud_id_t noeud_min = min_noeud_liste(t);
    printf("[%ld]\n", noeud_min);
    detruire_liste(&t);
}