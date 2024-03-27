#include "dijkstra.h"
#include <stdlib.h>

/**
 * construire_chemin_vers - Construit le chemin depuis le noeud de départ donné
 * vers le noeud donné. On passe un chemin en entrée-sortie de la fonction, qui
 * est mis à jour par celle-ci.
 *
 * Le noeud de départ est caractérisé par un prédécesseur qui vaut `NO_ID`.
 *
 * Ce sous-programme fonctionne récursivement :
 *  1. Si le noeud a pour précédent `NO_ID`, on a fini (c'est le noeud de
 *     départ, le chemin de départ à départ se compose du simple noeud départ)
 *  2. Sinon, on construit le chemin du départ au noeud précédent (appel
 *     récursif)
 *  3. Dans tous les cas, on ajoute le noeud au chemin, avec les
 *     caractéristiques associées dans visites
 *
 * @param chemin [in/out] chemin dans lequel enregistrer les étapes depuis le
 *                        départ vers noeud
 * @param visites [in] liste des noeuds visités créée par l'algorithme de
 *                     Dijkstra
 * @param noeud noeud vers lequel on veut construire le chemin depuis le départ
 */
static void construire_chemin_vers(liste_noeud_t *chemin,
                                   const liste_noeud_t *visites,
                                   noeud_id_t noeud) {
    noeud_id_t precedent = precedent_noeud_liste(visites, noeud);
    float distance = distance_noeud_liste(chemin, noeud);

    // On modifie le noeud dans chemin maintenant qu'on a toutes ses
    // caractéristiques.
    changer_noeud_liste(chemin, noeud, precedent, distance);

    if (precedent == NO_ID) {
        // on a fini, noeud est le noeud de départ
        return;
    } else {
        // Ajout du point sans précédant pour l'instant, on sait qu'on va le
        // calculer ensuite. On assignera alors la distance en même temps.
        inserer_noeud_liste(chemin, precedent, NO_ID, 0);

        // appel récursif, on étend le chemin vers le noeud de départ.
        construire_chemin_vers(chemin, visites, precedent);
    }
}

float dijkstra(const struct graphe_t *graphe,
               noeud_id_t source,
               noeud_id_t destination,
               liste_noeud_t **chemin) {
    liste_noeud_t *a_visiter = creer_liste();
    liste_noeud_t *visites = creer_liste();

    // ajout du noeud de départ
    inserer_noeud_liste(a_visiter, source, NO_ID, 0);

    while (!est_vide_liste(a_visiter)) {
        // noeud courant de distance minimale
        noeud_id_t nc = min_noeud_liste(a_visiter);

        // ajout nc dans visités
        noeud_id_t precedent = precedent_noeud_liste(a_visiter, nc);
        float dist_a_visiter = distance_noeud_liste(a_visiter, nc);
        inserer_noeud_liste(visites, nc, precedent, dist_a_visiter);

        // suppression nc dans à visiter
        supprimer_noeud_liste(a_visiter, nc);

        size_t nb_voisins = nombre_voisins(graphe, nc);

        if (nb_voisins != 0) {
            noeud_id_t *voisins = calloc(nb_voisins, sizeof(noeud_id_t));
            if (voisins == NULL) {
                // Échec de l'allocation dynamique
                printf("Échec de l'allocation dynamique !\n");
                return -1;
            }
            // printf("on a %ld voisins à traiter \n", nb_voisins);
            noeuds_voisins(graphe, nc, voisins);

            for (long unsigned int i = 0; i < nb_voisins; i++) {
                noeud_id_t nv = voisins[i];
                if (!contient_noeud_liste(visites, nv)) {
                    float dist_tot = distance_noeud_liste(visites, nc) +
                                     noeud_distance(graphe, nc, nv);  // delta'
                    float dist_act =
                        distance_noeud_liste(a_visiter, nv);  // delta
                    if (dist_tot < dist_act) {
                        // nc est meilleur précédent pour nv
                        changer_noeud_liste(a_visiter, nv, nc, dist_tot);
                    }
                }
            }
            free(voisins);
        }
    }

    detruire_liste(&a_visiter);

    if (chemin != NULL) {
        *chemin = creer_liste();
        construire_chemin_vers(*chemin, visites, destination);
    }

    float distance_totale = distance_noeud_liste(visites, destination);
    detruire_liste(&visites);
    return distance_totale;
}
