# Diagramme UML de la classe Cercle

| Cercle                                      |
|---------------------------------------------|
| **REQUÈTES**                                |
| - `centre : Point`                          |
| - `rayon : double`                          |
| - `couleur : Color`                         |
| `getCentre() : Point`                       |
| `getRayon() : double`                       |
| `getCouleur() : Color`                      |
| `getDiametre() : double`                    |
| `getPerimetre() : double` ($2\pi r$)        |
| `getAire() : double` ($\pi r^2$)            |
| `estDansCercle(p : Point) : Bool`           |
| `creerCercle(c, p : Point) : Cercle` (bleu) |
| **COMMANDES**                               |
| `setRayon(r : double)`                      |
| `setDiametre(d : double)`                   |
| `setCouleur(c : Color)`                     |
| `translater (dx, dy : double)`              |
| `afficher()` (Cr@(a, b))                    |
| **CONSTRUCTEURS**                           |
| `Cercle(c : Point, r : double)` (bleu)      |
| `Cercle(p1, p2 : Point)` (bleu)             |
| `Cercle(p1, p2 : Point, c : Color)`         |
