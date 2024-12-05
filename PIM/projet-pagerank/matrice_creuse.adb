with Ada.Text_IO;           use Ada.Text_IO;
with PageRank_Doc;          use PageRank_Doc;
with PageRank_Exceptions;   use PageRank_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body matrice_creuse is

    -- Instanciation de Text_IO pour lire des entiers
    package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
    use Int_IO;


    ----------------------------------------------------- Fonctions de T_Vecteur_Creux ----------------------------------------------------------------------------------------
    procedure Free is new Ada.Unchecked_Deallocation
       (T_Cellule, T_Vecteur_Creux);

    --Fonction pour initialiser
    procedure Initialiser (V : out T_Vecteur_Creux) is
    begin
        V := null;
    end Initialiser;

    --Fonction pour détruire un pointeur
    procedure Detruire (V : in out T_Vecteur_Creux) is
    begin
        if V /= null then
            Detruire (V.Suivant);
        end if;
        Free (V);

    end Detruire;

    --Fonction pour ajouter une valeur donnée a un indice donnée (ou modifier une valeur si l'indice est déja dans le vecteur creux)
    procedure Modifier
       (V : in out T_Vecteur_Creux; Indice : in Integer; Valeur : in T_Reel)
    is

        V_Temp : T_Vecteur_Creux;

        procedure aux (V : in out T_Vecteur_Creux) is
        begin
            if V = null then
                V := new T_Cellule'(Indice, Valeur, null);
            elsif V.all.Indice = Indice then
                V.all.Valeur := Valeur;
            elsif V.all.Indice < Indice then
                aux (V.all.Suivant);
            else
                V_Temp := V;
                V      := new T_Cellule'(Indice, Valeur, V_Temp);
            end if;
        end aux;
    begin
        aux (V);
    end Modifier;

    function Composante_Recursif
       (V : in T_Vecteur_Creux; Indice : in Integer) return T_Reel
    is
    begin

        if (V = null) then
            return (0.0);

        elsif (Indice < V.Indice) then
            return (0.0);

        elsif Indice = V.Indice then
            return (V.Valeur);

        else
            return (Composante_Recursif (V.Suivant, Indice));

        end if;
    end Composante_Recursif;

    ------------------------------------------------------- Fonctions de T_Vecteur ----------------------------------------------------------------------------------------

    --Norme au carré de la différence de deux vecteurs
    function Norme2Diff (V1, V2 : in T_Vecteur) return T_Reel is
        S : T_Reel := 0.0;
    begin
        for I in V1'Range loop
            S := S + ((V1 (I) - V2 (I)) * (V1 (I) - V2 (I)));
        end loop;
        return S;
    end Norme2Diff;

    --------------------------------------------------------------- Fonction de T_Matrice_Creuse --------------------------------------------------------------------------------

    --  function produit
    --     (V : in T_Vecteur; M : in T_Matrice_Creuse; alpha : in T_Reel;
    --      N : in Integer) return T_Vecteur
    --  is
    --      U : T_Vecteur(0..N-1);
    --      S       : T_Reel;
    --      J       : Integer;
    --      Courant : T_Vecteur_Creux;
    --      Vide    : Boolean;
    --      Dump    : constant T_Reel := ((1.0 - alpha) / T_Reel (N));
    --      Sommepi : T_Reel := 0.0;
    --  begin
    --      for I in 0..N-1 loop
    --          Sommepi := Sommepi + V(I);
    --      end loop;

    --      Sommepi := Sommepi*Dump;

    --      for I in 0..N-1 loop
    --          U(I) := Sommepi;
    --      end loop;

    --      for I in 0 .. N - 1 loop
    --          S       := 0.0;
    --          Courant := M(I);
    --          Vide    := False;
    --          J       := Courant.Indice;

    --          if Courant /= null then
    --              loop

    --                  S := S + V(J) * (Courant.Valeur);

    --                  --on vérifie si on est pas a la fin des valeurs
    --                  if Courant.Suivant = null then
    --                      Vide := True;
    --                  else
    --                      Courant := Courant.Suivant;
    --                  end if;
    --                  exit when Vide;
    --              end loop;

    --          else
    --              S := 0.0;
    --          end if;

    --          U(I) := U(I) + S;
    --      end loop;
    --      return(U);

    --  end produit;

    function produit
       (V : T_Vecteur; M : T_Matrice_Creuse; alpha : T_Reel; N : Integer)
        return T_Vecteur
    is
        U       : T_Vecteur (0 .. N - 1);
        S       : T_Reel;
        J       : Integer;
        J_pred  : Integer;
        Courant : T_Vecteur_Creux;
        Vide    : Boolean;
        Dump    : constant T_Reel := ((1.0 - alpha) / T_Reel (N));
    begin
        for I in 0 .. N - 1 loop
            S       := 0.0;
            Courant := M (I);
            Vide    := False;
            J       := 0;

            if Courant /= null then
                loop

                    J_pred := J;
                    J      := Courant.Indice;

                    if J /= 0 and J_pred = 0 and
                       Composante_Recursif (M (I), 0) = 0.0
                    then
                        J_pred := J_pred - 1;
                    end if;

                    for K in (J_pred + 1) .. (J - 1) loop

                        S := S + V (K) * Dump;

                    end loop;

                    S := S + V (J) * (Courant.Valeur + Dump);

                    if Courant.Suivant = null then
                        Vide := True;
                    else
                        Courant := Courant.Suivant;
                    end if;
                    exit when Vide;
                end loop;
                if J < N - 1 then
                    for K in J + 1 .. (N - 1) loop
                        S := S + V (K) * Dump;

                    end loop;

                end if;
            else
                for K in 0 .. N - 1 loop
                    S := S + V (K) * Dump;
                end loop;
            end if;

            U (I) := S;
        end loop;

        return (U);
    end produit;

    -- Produit d'une matrice par un réel
    procedure affine_creux
       (A : in out T_Matrice_Creuse; b : in T_Reel; N : Integer)
    is
        Courant : T_Vecteur_Creux;
    begin
        for I in 0 .. N - 1 loop
            Courant := A (I);
            while (Courant /= null) and then (Courant.Suivant /= null) loop
                Courant.Valeur := b * Courant.Valeur;
                Courant        := Courant.Suivant;
            end loop;
            if Courant /= null then
                Courant.Valeur := b * Courant.Valeur;
            end if;
        end loop;
    end affine_creux;

    -- Construit alpha * S (le reste de l'expression de G étant présente dans la fonction produit)
    procedure Construction_G_creux
       (G : in out T_Matrice_Creuse; Alpha : T_Reel; Nom_Fichier : in String;
        N :        Integer)
    is
        Ligne_Vide     : T_Vecteur (0 .. N - 1);
        Courant        : T_Vecteur_Creux;

    begin
        -- on appelle extraire pour obtenir H
        Extraire (G, Nom_Fichier, N);
        Ligne_Vide := (others => 1.0);
        -- On parcourt H une première fois pour savoir quelle ligne sont vide
        for I in 0 .. N - 1 loop
            Courant := G (I);
            while (Courant /= null) loop
                if Courant.Valeur /= 0.0 then
                    Ligne_Vide (Courant.Indice) := 0.0;
                end if;
                Courant := Courant.Suivant;
            end loop;
        end loop;

        --On reparcourt pour remplir les lignes Vides
        for I in 0 .. N - 1 loop
            if Ligne_Vide (I) = 1.0 then
                for J in 0 .. N - 1 loop
                    Modifier (G (J), I, 1.0 / T_Reel (N));
                end loop;
            end if;
        end loop;
        affine_creux (G, Alpha, N);
    end Construction_G_creux;

    -- Prend une String composée de deux entier (un arc) et renvoie les deux entiers
    procedure Extraire_Arc
       (Arc : in String; S1, S2 : out Integer; N : in Integer)
    is
        Fin : Integer;
    begin
        -- Lit le premier entier trouvé dans Arc, puis le premier après Fin
        Get (Arc, S1, Fin);
        Get (Arc (Fin + 1 .. Arc'Last), S2, Fin);

        if (S1 >= N) or else (S2 >= N) or else (S1 < 0) or else (S2 < 0) then
            raise Constraint_Error;
        end if;

    exception

        when Data_Error | Fichier_Format_Exception => -- Format invalide
            Afficher_Exemple_Graphe;
            raise Fichier_Format_Exception
               with """" & Arc & """" &
               " est du mauvais format pour représenter un arc. ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";

        when Constraint_Error => -- noeuds dans le mauvais intervalle
            Afficher_Exemple_Graphe;
            raise Fichier_Format_Exception
               with """" & Arc & """" &
               " est un arc dont l'indice des noeuds est invalide. ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";

    end Extraire_Arc;

    --Construit la matrice H a partir du fichier .net
    procedure Extraire
       (G : out T_Matrice_Creuse; Nom_Fichier : in String; N : in Integer)
    is
        Fichier   : File_Type;
        Ligne     : Unbounded_String;
        S1, S2    : Integer;
        Par_Ligne : T_Vecteur (0 .. N - 1) := (others => 0.0);
        Courant   : T_Vecteur_Creux;
    begin
        Open (File => Fichier, Mode => In_File, Name => Nom_Fichier);
        -- On lit la première ligne sans rien faire avec (on connaît déjà le nombe de noeuds)
        Ligne := To_Unbounded_String (Get_Line (File => Fichier));

        while not End_Of_File (Fichier) loop

            -- Lire la ligne, extraire les sommets de l'arc et stocker l'arc dans H
            Ligne := To_Unbounded_String (Get_Line (File => Fichier));
            Extraire_Arc (To_String (Ligne), S1, S2, N);
            Modifier (G (S2), S1, 1.0);
            Par_Ligne (S1) := Par_Ligne (S1) + 1.0;

        end loop;

        for I in 0 .. N - 1 loop
            Courant := G (I);
            while Courant /= null loop
                Courant.Valeur := Courant.Valeur / Par_Ligne (Courant.Indice);
                Courant        := Courant.Suivant;
            end loop;

        end loop;

        Close (Fichier);

    end Extraire;

    -- Fonction qui renvoie en sortie le vecteur résultat
    function calcul_creux
       (Nom_Fichier : in String; N : in Integer; Max_Iter : in out Integer;
        Alpha, Eps  : in T_Reel) return T_Vecteur

    is
        G : T_Matrice_Creuse (0 .. N - 1);
    begin
        Construction_G_creux (G, Alpha, Nom_Fichier, N);

        return (Calcul_creux_aux (G, N, Max_Iter, Alpha, Eps));
    end calcul_creux;

    --Fonction auxilliaire qui permet d'éviter d'avoir trop de vecteur de trop grosse taille ouvert (285000 pour linux26) en meme temps
    function Calcul_creux_aux
       (G          : in out T_Matrice_Creuse; N : in Integer; Max_Iter : in out Integer;
        Alpha, Eps : in     T_Reel) return T_Vecteur
    is
        Exit_Flag : Integer := 0;
        Iter      : Integer := 0;
        Pi, Temp  : T_Vecteur (0 .. N - 1);
    begin
        Pi := (others => 1.0 / T_Reel (N));

        --Calculs successifs des poids

        while Exit_Flag = 0 loop
            Temp := Pi;
            Pi   := produit (Pi, G, Alpha, N);
            Iter := Iter + 1;

            --Vérifier les cas d'arret
            if Iter >= Max_Iter then

                Exit_Flag := 1;
            elsif Eps /= 0.0 then
                if Norme2Diff (Pi, Temp) < Eps**2 then
                    Exit_Flag := 2;
                end if;
            end if;
        end loop;

        for I in 0 .. N - 1 loop
            Detruire (G (I));
        end loop;

        Max_Iter := Iter;

        return (Pi);

    end Calcul_creux_aux;

end matrice_creuse;