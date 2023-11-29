// This file is part of mandelbrot.
//
// mandelbrot is free software: you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.
//
// mandelbrot is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// mandelbrot. If not, see <https://www.gnu.org/licenses/>.
//
// mandelbrot - Copyright (c) 2023 Guillaume Dupont
// Contact: <guillaume.dupont@toulouse-inp.fr>
#include "mandelbrot.h"

int mandelbrot(complexe_t z0, complexe_t c, double seuil, int maxit)
{
    int nbit = 0;

    complexe_t zn;
    copie(&zn, z0);

    while ((module(zn) <= seuil) && (nbit < maxit))
    {
        // Calcul du terme suivant (stocké dans zn)
        puissance(&zn, zn, 2);
        ajouter(&zn, zn, c);

        nbit++;
    }

    return nbit;

    // 1. On calcule un à un les termes de la suite de Mandelbrot(en suivant la relation de récurrence présentée plus haut)

    // 2. Dès que le module du terme actuel dépasse seuil, on s’arrête

    // 3. Dès que le nombre d’itérations effectuées dépasse maxit, on s’arrête

    // 4. On retourne le nombre d’itérations effectuées(qui doit donc se trouver entre 0 et maxit inclus)
}
