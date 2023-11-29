#ifndef COMPLEX_H
#define COMPLEX_H

// Type utilisateur complexe_t
struct complexe_t
{
    float r; // Partie réelle
    float i; // Partie imaginaire
};

typedef struct complexe_t complexe_t;

// Fonctions reelle et imaginaire
/**
 * reelle
 * Cette fonction renvoie la partie réelle d'un complexe
 *
 * Paramètres :
 *  z    complexe dont on veut la partie réelle
 *
 * Retour :
 *  z.r  partie réelle de z
 */
float reelle(complexe_t z);

/**
 * imaginaire
 * Cette fonction renvoie la partie imaginaire d'un complexe
 *
 * Paramètres :
 *  z    complexe dont on veut la partie imaginaire
 *
 * Retour :
 *  z.r  partie imaginaire de z
 */
float imaginaire(complexe_t z);

// Procédures set_reelle, set_imaginaire et init
/**
 * set_reelle
 * Cette fonction modifie la partie réelle de z avec x
 *
 * Paramètres :
 *  z   pointeur sur le complexe dont on veut modifier la partie réelle
 *  x   réel qu'on veut attribuer à la partie réelle de z
 */
void set_reelle(complexe_t *z, float x);

/**
 * set_imaginaire
 * Cette fonction modifie la partie imaginaire de z avec y
 *
 * Paramètres :
 *  z   pointeur sur le complexe dont on veut modifier la partie imaginaire
 *  y   réel qu'on veut attribuer à la partie imaginaire de z
 */
void set_imaginaire(complexe_t *z, float y);

/**
 * init
 * Cette fonction modifie les parties réelle et imaginaire de z avec x et y
 *
 * Paramètres :
 *  z   pointeur sur le complexe dont on veut modifier les parties réelle et imaginaire
 *  x   réel qu'on veut attribuer à la partie réelle de z
 *  y   réel qu'on veut attribuer à la partie imaginaire de z
 */
void init(complexe_t *z, float x, float y);

// Procédure copie
/**
 * copie
 * Copie les composantes du complexe donné en second argument dans celles du premier
 * argument
 *
 * Paramètres :
 *   resultat       [out] Complexe dans lequel copier les composantes
 *   autre          [in]  Complexe à copier
 *
 * Pré-conditions : resultat non null
 * Post-conditions : resultat et autre ont les mêmes composantes
 */
void copie(complexe_t *resultat, complexe_t autre);

// Algèbre des nombres complexes
/**
 * conjugue
 * Calcule le conjugué du nombre complexe op et le sotocke dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   op             [in]  Complexe dont on veut le conjugué
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : reelle(*resultat) = reelle(op), complexe(*resultat) = - complexe(op)
 */
void conjugue(complexe_t *resultat, complexe_t op);

/**
 * ajouter
 * Réalise l'addition des deux nombres complexes gauche et droite et stocke le résultat
 * dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   gauche         [in]  Opérande gauche
 *   droite         [in]  Opérande droite
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = gauche + droite
 */
void ajouter(complexe_t *resultat, complexe_t gauche, complexe_t droite);

/**
 * soustraire
 * Réalise la soustraction des deux nombres complexes gauche et droite et stocke le résultat
 * dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   gauche         [in]  Opérande gauche
 *   droite         [in]  Opérande droite
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = gauche - droite
 */
void soustraire(complexe_t *resultat, complexe_t gauche, complexe_t droite);

/**
 * multiplier
 * Réalise le produit des deux nombres complexes gauche et droite et stocke le résultat
 * dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   gauche         [in]  Opérande gauche
 *   droite         [in]  Opérande droite
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = gauche * droite
 */
void multiplier(complexe_t *resultat, complexe_t gauche, complexe_t droite);

/**
 * echelle
 * Calcule la mise à l'échelle d'un nombre complexe avec le nombre réel donné (multiplication
 * de op par le facteur réel facteur).
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   op             [in]  Complexe à mettre à l'échelle
 *   facteur        [in]  Nombre réel à multiplier
 *
 * Pré-conditions : resultat non-null
 * Post-conditions : *resultat = op * facteur
 */
void echelle(complexe_t *resultat, complexe_t op, double facteur);

/**
 * puissance
 * Calcule la puissance entière du complexe donné et stocke le résultat dans resultat.
 *
 * Paramètres :
 *   resultat       [out] Résultat de l'opération
 *   op             [in]  Complexe dont on veut la puissance
 *   exposant       [in]  Exposant de la puissance
 *
 * Pré-conditions : resultat non-null, exposant >= 0
 * Post-conditions : *resultat = op * op * ... * op
 *                                 { n fois }
 */
void puissance(complexe_t *resultat, complexe_t op, int exposant);

// Module et argument
/**
 * module_carre
 * calcule le carré du module du complexe donné en paramètre
 *
 * Paramètres :
 *  z   complexe dont on veut le carré du module
 * Retour :
 *  carré du module de z
 */
float module_carre(complexe_t z);

/**
 * module
 * calcule le module du complexe donné en paramètre
 *
 * Paramètres :
 *  z   complexe dont on veut le module
 * 
 * Retour :
 *  module de z
 */
float module(complexe_t z);

/**
 * argument
 * calcule l'argument du complexe donné en paramètre
 *
 * Paramètres :
 *  z   complexe dont on veut l'argument
 *
 * Retour :
 *  argumant de z
 * 
 * Pré-condition : z != 0
 */
float argument(complexe_t z);

#endif // COMPLEXE_H
