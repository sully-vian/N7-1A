-- Implémentation de structures de données vecteur et matrice pleins
-- Implémentation de l'algorithme de PageRank

generic

    type T_Reel is digits <>;

package Matrice_Pleine is

    -- * -------------------- DÉFINITION DES TYPES --------------------

    type T_Vecteur is array (Integer range <>) of T_Reel;
    type T_Matrice is array (Integer range <>, Integer range <>) of T_Reel;

    -- * -------------------- OPÉRATIONS SUR LES VECTEURS --------------------

    -- Carré de la norme euclidienne d'un vecteur
    function Norme2 (V : in T_Vecteur) return T_Reel;

    -- Différence vecteur - vecteur
    function "-" (V1, V2 : in T_Vecteur) return T_Vecteur;

    -- * -------------------- OPÉRATIONS SUR LES MATRICES --------------------

    -- Somme matrice + matrice
    function "+" (A, B : in T_Matrice) return T_Matrice;

    function "*" (A, B : in T_Matrice) return T_Matrice;

    -- Produit scalaire * matrice
    function "*" (x : in T_Reel; A : in T_Matrice) return T_Matrice;

    procedure Exponentiation_Rapide (M : in out T_Matrice; K : in Integer);

    -- * -------------------- OPÉRATIONS TRANSVERSES --------------------

    -- produit vecteur * matrice
    function "*" (V : in T_Vecteur; M : in T_Matrice) return T_Vecteur;

    -- * -------------------- OPÉRATIONS DU CALCUL DE PAGERANK --------------------

    -- Calcule puis renvoie S dans H
    procedure Calcul_S (M : in out T_Matrice);

    -- Calcule puis Renvoie G dans M
    procedure Calcul_G (M : in out T_Matrice; Alpha : in T_Reel);

    -- Extrait les noms des sommets d'un arc entré en chaine de caractères
    procedure Extraire_Arc
       (Arc : in String; S1, S2 : out Integer; N : in Integer);

    -- Renvoie la matrice d'adjacence correspondant au fichier donné en entrée avec le nombre de noeuds du graphe
    procedure Extraire
       (H    : in out T_Matrice; Nom_Fichier : in String; N : in Integer;
        Temp : in out T_Vecteur);

    function Calcul_Plein
       (Nom_Fichier : in String; N : in Integer; Max_Iter : in out Integer;
        Alpha, Eps  : in T_Reel) return T_Vecteur;

end Matrice_Pleine;
