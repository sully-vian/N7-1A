with Ada.Text_IO;           use Ada.Text_IO;
with PageRank_Doc;          use PageRank_Doc;
with PageRank_Exceptions;   use PageRank_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;

package body Matrice_Pleine is

    -- Instanciation de Text_IO pour lire des entiers
    package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
    use Int_IO;

    ---------------------- OPÉRATIONS SUR LES VECTEURS --------------------

    function Norme2 (V : in T_Vecteur) return T_Reel is
        S : T_Reel := 0.0;
    begin
        for I in V'Range loop
            S := S + V (I) * V (I);
        end loop;
        return S;
    end Norme2;

    function "-" (V1, V2 : in T_Vecteur) return T_Vecteur is
        V3 : T_Vecteur (V1'Range);
    begin
        for I in V1'Range loop
            V3 (I) := V1 (I) - V2 (I);
        end loop;
        return V3;
    end "-";

    ---------------------- OPÉRATIONS SUR LES MATRICES --------------------

    function "+" (A, B : in T_Matrice) return T_Matrice is
        N : constant Integer := A'Length;
        C : T_Matrice (0 .. N - 1, 0 .. N - 1);
    begin
        for I in 0 .. N - 1 loop
            for J in 0 .. N - 1 loop
                C (I, J) := A (I, J) + B (I, J);
            end loop;
        end loop;
        return C;
    end "+";

    function "*" (A, B : in T_Matrice) return T_Matrice is
        N : constant Integer := A'Length;
        C : T_Matrice        := (0 .. N - 1 => (0 .. N - 1 => 0.0));
    begin
        for I in 0 .. N - 1 loop
            for J in 0 .. N - 1 loop
                for K in 0 .. N - 1 loop
                    C (I, J) := C (I, J) + A (I, K) * B (K, J);
                end loop;
            end loop;
        end loop;
        return C;
    end "*";

    function "*" (x : in T_Reel; A : in T_Matrice) return T_Matrice is
        N : constant Integer := A'Length;
        C : T_Matrice (0 .. N - 1, 0 .. N - 1);
    begin
        for I in 0 .. N - 1 loop
            for J in 0 .. N - 1 loop
                C (I, J) := A (I, J) * x;
            end loop;
        end loop;
        return C;
    end "*";

    procedure Exponentiation_Rapide (M : in out T_Matrice; K : in Integer) is
        N : constant Integer := M'Length;
        B : T_Matrice        := (0 .. N - 1 => (0 .. N - 1 => 0.0));
    begin
        if (K = 1) then
            null;
        elsif (K = 2) then
            M := M * M;
        elsif (K mod 2 = 0) then
            B := M;
            Exponentiation_Rapide (B, K / 2);
            M := B * B;
        else
            B := M;
            Exponentiation_Rapide (B, (K - 1) / 2);
            B := B * B;
            M := M * B;
        end if;
    end Exponentiation_Rapide;

    -- * -------------------- OPÉRATIONS TRANSVERSES --------------------

    function "*" (V : T_Vecteur; M : T_Matrice) return T_Vecteur is
        U : T_Vecteur (V'Range);
        S : T_Reel;
    begin
        for I in V'Range loop
            S := 0.0;
            for J in V'Range loop
                S := S + V (J) * M (J, I);
            end loop;
            U (I) := S;
        end loop;
        return U;
    end "*";

    -- * -------------------- OPÉRATIONS DU CALCUL DE PAGERANK --------------------

    procedure Calcul_S (M : in out T_Matrice) is
        N : constant Integer := M'Length;
        J : Integer;
    begin
        for I in 0 .. N - 1 loop -- pour chaque ligne

            -- vérifier de la nullité de la ligne I
            J := 0; -- indice de la colonne
            while (J < N) loop
                if (M (I, J) = 0.0) then
                    J := J + 1;
                else
                    -- on force l'arrêt de la boucle TANT QUE dès qu'on sait que la ligne I est non nulle
                    J := N + 1;
                end if;
            end loop;

            -- cas de ligne nulle : J a atteint N, on remplace cette ligne
            if (J = N) then
                for J in 0 .. N - 1 loop
                    M (I, J) := 1.0 / T_Reel (N);
                end loop;
            end if;
        end loop;
    end Calcul_S;

    procedure Calcul_G (M : in out T_Matrice; Alpha : in T_Reel) is
        N : constant Integer := M'Length;
    begin
        for I in 0 .. N - 1 loop
            for J in 0 .. N - 1 loop

                M (I, J) := Alpha * M (I, J) + (1.0 - Alpha) / T_Reel (N);

            end loop;
        end loop;
    end Calcul_G;

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
        when Data_Error | Fichier_Format_Exception
           | Ada.IO_Exceptions.End_Error => -- Format invalide
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

    procedure Extraire
       (H    : in out T_Matrice; Nom_Fichier : in String; N : in Integer;
        Temp : in out T_Vecteur)
    is
        Fichier : File_Type;
        Ligne   : Unbounded_String;
        S1, S2  : Integer;
    begin
        Open (File => Fichier, Mode => In_File, Name => Nom_Fichier);

        -- On lit la première ligne sans rien faire avec (on connaît déjà le nombe de noeuds)
        Ligne := To_Unbounded_String (Get_Line (Fichier));

        while not End_Of_File (Fichier) loop
            -- Lire la ligne, extraire les sommets de l'arc et stocker l'arc dans H
            Ligne := To_Unbounded_String (Get_Line (Fichier));
            Extraire_Arc (To_String (Ligne), S1, S2, N);

            if (H (S1, S2) = 0.0) then -- un arc double compté pour un seul
                Temp (S1) := Temp (S1) + 1.0;
            end if;
            H (S1, S2) := 1.0;
        end loop;

        Close (Fichier);

        -- Remplacement des aecs par leur poids normalisé
        -- on obtient une matrice (presque) stochastique
        for I in 0 .. N - 1 loop
            if (Temp (I) /= 0.0) then
                for J in 0 .. N - 1 loop
                    H (I, J) := H (I, J) / Temp (I);
                end loop;
            end if;
        end loop;

    end Extraire;

    -- Calcule le PageRank
    function Calcul_Plein
       (Nom_Fichier : in String; N : in Integer; Max_Iter : in out Integer;
        Alpha, Eps  : in T_Reel) return T_Vecteur
    is
        Exit_Flag : Integer   := 0;
        Iter      : Integer   := 0;
        M         : T_Matrice := (0 .. N - 1 => (0 .. N - 1 => 0.0));
        Pi, Temp  : T_Vecteur := (0 .. N - 1 => 0.0);
        -- Temp a deux utilités : stocker le nombre d'arcs sortant par noeud dans Extraire et mémoriser la valeur de Pi dans la boucle TANT QUE
    begin

        -- Calcul de G
        Extraire (M, Nom_Fichier, N, Temp); -- M contient H après cette étape
        Calcul_S (M); -- M contient S après cette étape
        Calcul_G (M, Alpha); -- M contient G après cette étape

        -- Initialisation de Pi
        Pi := (others => 1.0 / T_Reel (N));

        if (Eps > 0.0) then
            -- Calculs successifs des poids
            while (Exit_Flag = 0) loop
                Temp := Pi;
                Pi   := Pi * M;
                Iter := Iter + 1;

                -- Vérifier les cas d'arret
                if (Iter >= Max_Iter) then
                    Exit_Flag := 1;
                elsif (Norme2 (Pi - Temp) < Eps**2) then
                    Exit_Flag := 2;
                end if;

            end loop;

            -- Max_Iter est en in out, le modifier permet de récupérer sa valeurpour l'afficher dans le fichier sortie en .prw
            Max_Iter := Iter;

        else
            -- calcul de Pi par exponentiation rapide de G (on connaît déjà le nombre exact d'itérations)
            Exponentiation_Rapide (M, Max_Iter);
            -- M contient maintenant G^K
            Pi := Pi * M;
        end if;

        return Pi;

    end Calcul_Plein;

end Matrice_Pleine;
