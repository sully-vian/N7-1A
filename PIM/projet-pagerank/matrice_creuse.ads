generic
   type T_Reel is digits <>;

package Matrice_Creuse is

   ------------------------------------------------------------------------- DEFINITION DES TYPES------------------------------------------------

   type T_Cellule;

   type T_Vecteur_Creux is access T_Cellule;

   type T_Cellule is record
      Indice  : Integer;
      Valeur  : T_Reel;
      Suivant : T_Vecteur_Creux;
   end record;

   type T_Matrice_Creuse is array (Integer range <>) of T_Vecteur_Creux;
   type T_Vecteur is array (Integer range <>) of T_Reel;

   ---------------------------------------------------------FONCTIONS DE T_Vecteur_Creux----------------------------------------------------------------

   -- Initialiser un vecteur creux.  Il est nul.
   procedure Initialiser (V : out T_Vecteur_Creux);

   -- Détruire le vecteur V.
   procedure Detruire (V : in out T_Vecteur_Creux);


   procedure Modifier
     (V : in out T_Vecteur_Creux; Indice : in Integer; Valeur : in T_Reel);

    function Composante_Recursif (V : in T_Vecteur_Creux ; Indice : in Integer) return T_Reel;


   --------------------------------------------------------------FONCTION DE T_Vecteur ET T_Matrice_Creuse------------------------------------------

   function Norme2Diff (V1, V2 : in T_Vecteur) return T_Reel;

       function produit
       (V : in T_Vecteur; M : in T_Matrice_Creuse; alpha : in T_Reel;
        N : in     Integer) return T_Vecteur;

   procedure affine_creux
     (A : in out T_Matrice_Creuse; b : in T_Reel; N : Integer);

   procedure Construction_G_creux
     (G : in out T_Matrice_Creuse; Alpha : T_Reel; Nom_Fichier : in String; N : Integer);

   procedure Extraire_Arc (Arc : in String; S1, S2 : out Integer; N : in Integer);

   procedure Extraire (G : out T_Matrice_Creuse; Nom_Fichier : in String; N : in Integer);

   function Calcul_Creux
     (Nom_Fichier : in String; N : in Integer; Max_Iter : in out Integer;
      Alpha, Eps  : in T_Reel) return T_Vecteur;
  
  function Calcul_creux_aux
       (G : in out T_Matrice_Creuse; N : in Integer; Max_Iter : in out Integer;
        Alpha, Eps  : in T_Reel) return T_Vecteur;
end Matrice_Creuse;
