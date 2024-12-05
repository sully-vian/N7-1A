# Documentation

## Général

- $N\in\mathbb{N}$ : nombre de pages à Classer
- $|P_i|\in\mathbb{N}$ : nombre d'hyperliens de la page $P_i$
- $P_i^{IN}$ : ensemble des pages référençant la page $P_i$
  - Exemple : $P_j\in P_i^{IN} \Leftrightarrow "P_j\text{ référence } P_i"$

## Poids d'une page

Si $P_i$ est une page, on définie récursivement son poids :

$$ r(P_i) = \sum_{P_j \in P_i^{IN}}\frac{r(P_j)}{|P_j|} $$

### Algorithme de calcul

Soit $r_k(P_i)$ le poids de $P_i$ à l'itération $k$ :

$$ r_{k+1} = \sum_{P_j \in P_i^{IN}}\frac{r_k(P_j)}{|P_j|} $$

avec $r_0(P_i) = 1/N$

$\Big(r_k(P_i)\Big)_{k\in\mathbb{N}}$ converge vers $r(P_i)$

## Modélisation en graphe orienté

Les pages web sont les noeuds \
Les hyperliens sont les liens orientés : $P_j \in P_i^{IN} \Leftrightarrow \Big(P_j \rightarrow P_i\Big)$

### Définitions

- $\pi_k^T = \Big(r_k(P_0)\ ...\ r_k(P_{N-1})\Big)$ : vecteur ligne des poids au rang $k$
- $H$ : matrice d'adjacence pondérée telle que $H_{i,j} = \left\{\begin{array}{cl}1/|P_i| & \text{si } P_i\in P_j^{IN}\\ 0 & \text{ sinon}\end{array}\right.$
  - Rmq : la somme d'une ligne de $H$ vaut $1$ : $\sum\limits_{i=0}^{N-1}H_{i,j} = 1$

Pour répartir les poids et converger, on modifie $H$ deux fois : en $S$, puis en $G$

- $S$ : Si un $P_i$ est un cul de sac (aucun hyperlien), on attribue $1/N$ à tous les termes de $H$ correspondants
- $\alpha\in[0,1]$ : _dumping factor_, paramètre utilisateur (usuellement $0.85$)
  - Plus il est proche de $1$, plus le calcul est précis
  - Plus il est proche de $0$, plus la convergence est rapide
- $G = \alpha\cdot S + \frac{1-\alpha}{N}ee^T$ : version à paramètre de $H$ modifiée qui permet de contrôler l'algorithme'

### Algorithme

On a donc (avec $G$ qui prend le rôle de $H$)

$$\pi_{k+1}^T = \pi_k^T\cdot G$$

avec $\pi_0^T = \big(\frac{1}{N}\ ...\ \frac{1}{N}\big)$

## Représentation textuelle d'un graphe

### Fichiers

- **Fichier Graphe** (`.net`) : décrit le graphe
- **Fichier PageRank** (`.pr`) : classement des noeuds suivant l'algo
- **Fichier Poids** (`.prw`) : poids des noeuds

#### Fichier Graphe (`.net`)

- 1ère ligne: nombre de noeuds
  - `6` signifie que le graphe comporte 6 noeuds
- lignes suivantes : arcs orientée
  - `3 0` signifie que le graphe comporte un arc du noeud $3$ au noeud $0$
- il existe plusieurs manières de représenter un même graphe

#### Fichier PageRank (`.pr`)

- les rangs sont écrits dans l'ordre des noeuds
- ils ont été écrits avec la procédure `Put (X, Fore => 1, Exp => 0)`. On évite ainsi les espaces en trop.
  - `Fore => 1` indique d'utiliser au moins `1` case pour la partie entière
  - `Exp => 0` indique d'utiliser `0` chiffres pour l'exposant
- La précision de calcul est de $15$ chiffres (`Long_Float`)

#### Fichier Poids (`.prw`)

- les poids sont écrits dans l'ordre de définition des arcs dans le **Fichier Graphe**
