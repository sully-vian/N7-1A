#ifndef TAB_H
#define TAB_H

// Type abstrait tab_t
struct tab_t;
typedef struct tab_t tab_t;

// creer
tab_t *creer();

// detruire
void detruire(tab_t **tab);

// ajouter
void ajouter(tab_t *tab, int elt);

// supprimer
void supprimer(tab_t *tab, int elt);

// element
int element(const tab_t *tab, int id);

// taille
int taille(const tab_t *tab);

// espace
int espace(const tab_t *tab);

// serrer
void serrer(tab_t *tab);

#endif
