#include "complexe.h"
#include <math.h> // Pour certaines fonctions trigo notamment

// Implantations de reelle et imaginaire
float reelle(complexe_t z)
{
    return z.r;
}

float imaginaire(complexe_t z)
{
    return z.i;
}

// Implantations de set_reelle et set_imaginaire
void set_reelle(complexe_t *z, float x)
{
    z->r = x;
}

void set_imaginaire(complexe_t *z, float y)
{
    z->i = y;
}

void init(complexe_t *z, float x, float y)
{
    z->r = x;
    z->i = y;
}

// Implantation de copie
void copie(complexe_t *resultat, complexe_t autre)
{
    *resultat = autre;
}

// Implantations des fonctions algébriques sur les complexes
void conjugue(complexe_t *resultat, complexe_t op)
{
    // init(resultat, op.r, -op.i);
    resultat->r = op.r;
    resultat->i = -op.i;
}

void ajouter(complexe_t *resultat, complexe_t gauche, complexe_t droite)
{
    resultat->r = gauche.r + droite.r;
    resultat->i = gauche.i + droite.i;
}

void soustraire(complexe_t *resultat, complexe_t gauche, complexe_t droite)
{
    resultat->r = gauche.r - droite.r;
    resultat->i = gauche.i - droite.i;
}

void multiplier(complexe_t *resultat, complexe_t gauche, complexe_t droite)
{
    resultat->r = gauche.r * droite.r - gauche.i * droite.i;
    resultat->i = gauche.r * droite.i - gauche.i * droite.r;
}

void echelle(complexe_t *resultat, complexe_t op, double facteur)
{
    resultat->r = op.r * facteur;
    resultat->i = op.i * facteur;
}

void puissance(complexe_t *resultat, complexe_t op, int exposant)
{
    if (exposant == 1)
    {
        *resultat = op;
    }
    else
    {
        complexe_t temp; // on stocke le résultat intermédiaire (récursif)
        puissance(&temp, op, exposant - 1);
        multiplier(resultat, op, temp);
    }
}

// Implantations du module et de l'argument
float module_carre(complexe_t z)
{
    return z.r * z.r + z.i * z.i;
}

float module(complexe_t z)
{
    return sqrt(z.r * z.r + z.i * z.i);
}

float argument(complexe_t z)
{
    return atan2(z.i, z.r);
}
