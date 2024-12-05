with Ada.Text_IO;           use Ada.Text_IO;
with PageRank_Doc;          use PageRank_Doc;
with Ada.Command_Line;      use Ada.Command_Line;
with PageRank_Exceptions;   use PageRank_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.IO_Exceptions;

with Matrice_Pleine;
with Matrice_Creuse;

procedure PageRank is

    -- Type des poids (type réel custom)
    PRECISION : constant Integer := 15;
    type T_Double is digits PRECISION;
    subtype T_Reel is T_Double;

    -- Instanciation des packages d'entrée/sortie
    package Reel_IO is new Ada.Text_IO.Float_IO (T_Reel);
    package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);
    use Reel_IO;
    use Integer_IO;

    -- Instanciation des packages de calcul
    package PR_Matrice_Creuse is new Matrice_Creuse (T_Reel);
    package PR_Matrice_Pleine is new Matrice_Pleine (T_Reel);
    use PR_Matrice_Creuse;
    use PR_Matrice_Pleine;

    -- Options
    A_Val : T_Reel           := 0.85; -- valeur de alpha
    K_Val : Integer          := 150; -- itération max
    E_Val : T_Reel           := 0.0; -- epsilon
    C_Val : Boolean          := True; -- implémentation matrice pleine/creuse
    R_Val : Unbounded_String := To_Unbounded_String ("output");

    -- procédure qui récupère les options et stocke leurs valeurs
    procedure Get_Args (Nom_Fichier : out Unbounded_String) is
        I : Integer := 1;
    begin
        while (I < Argument_Count) loop
            if (Argument (I) (1) = '-') then
                case Argument (I) (2) is

                    when 'A' => -- traitement de l'option A, lecture d'un réel (argument suivant)
                        begin
                            I     := I + 1;
                            A_Val := T_Reel'Value (Argument (I));

                            if (A_Val > 1.0) or (A_Val < 0.0) then
                                raise Constraint_Error;
                            end if;
                        exception
                            when Constraint_Error =>
                                New_Line;
                                Afficher_A_Doc;
                                raise Arg_Option_Exception
                                   with "A = " & Argument (I) &
                                   " ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";
                        end;

                    when 'K' => -- traitement de l'option K, lecture d'un entier (argument suivant)
                        begin
                            I     := I + 1;
                            K_Val := Integer'Value (Argument (I));

                            if (K_Val < 0) then
                                raise Constraint_Error;
                            end if;
                        exception
                            when Constraint_Error =>
                                New_Line;
                                Afficher_K_Doc;
                                raise Arg_Option_Exception
                                   with "K = " & Argument (I) &
                                   " ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";
                        end;

                    when 'E' => -- traitement de l'option E, lecture d'un réel (argument suivant)
                        begin
                            I     := I + 1;
                            E_Val := T_Reel'Value (Argument (I));

                            if (E_Val < 0.0) then
                                raise Constraint_Error;
                            end if;
                        exception
                            when Constraint_Error =>
                                New_Line;
                                Afficher_E_Doc;
                                raise Arg_Option_Exception
                                   with "E = " & Argument (I) &
                                   " ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";
                        end;

                    when 'C' => -- traitement de l'option C
                        C_Val := True;

                    when 'P' => -- traitement de l'option P
                        C_Val := False;

                    when 'R' => -- traitement de l'option R, lecture d'une chaîne de caractères (argument suivant)
                        begin
                            I     := I + 1;
                            R_Val := To_Unbounded_String (Argument (I));
                        exception
                            when Constraint_Error =>
                                New_Line;
                                Afficher_R_Doc;
                                raise Arg_Option_Exception
                                   with "R = " & Argument (I) &
                                   " ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";
                        end;

                    when 'H' => -- traitement de l'option H affichage de l'aide et sortie
                        raise Afficher_Doc_Exception; -- levée de l'xception associée
                    when others => -- cas d'option inconnue
                        raise Arg_Option_Exception
                           with "Option inconnue : """ & Argument (I) &
                           """ ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";

                end case;
            else
                raise Arg_Option_Exception with "Option inconnue : " & Argument (I);
            end if;
            I := I + 1;
        end loop;

        -- récupération du nom du fichier d'entrée
        Nom_Fichier := To_Unbounded_String (Argument (I));

    end Get_Args;

    -- Procédure générique qui trie le vecteur V et écrit les résultats dans deux fichiers texte
    generic
        type T_Vecteur is array (Integer range <>) of T_Reel;
    procedure Ecrire
       (V : in T_Vecteur; Nom_Fichier : String; Alpha : in T_Reel;
        K : in Integer);

    procedure Ecrire
       (V : in T_Vecteur; Nom_Fichier : String; Alpha : in T_Reel;
        K : in Integer)
    is

        -- Structure de donnée stockant les indices des pages
        type T_Vecteur_Ind is array (Integer range <>) of Integer;

        -- renvoie V trié par insertion par ordre décroissant
        procedure Tri_Insertion
           (V_Trie : in out T_Vecteur; Ind_Trie : out T_Vecteur_Ind)
        is
            N                 : constant Integer := V_Trie'Length;
            Ind_Max, Temp_Ind : Integer;
            Temp              : T_Reel;
        begin

            -- Initialisation du vecteur des indices de pages
            for I in 0 .. N - 1 loop
                Ind_Trie (I) := I;
            end loop;

            for I in 0 .. N - 1 loop
                -- Recherche de l'indice du max dans V_Trie (I .. N)
                Ind_Max := I;
                for J in I .. N - 1 loop
                    if V_Trie (J) > V_Trie (Ind_Max) then
                        Ind_Max := J;
                    end if;
                end loop;
                -- Mise à jour des poids dans
                Temp               := V_Trie (I);
                V_Trie (I)         := V_Trie (Ind_Max);
                V_Trie (Ind_Max)   := Temp;
                -- Mise à jour du tableau des indices de pages
                Temp_Ind           := Ind_Trie (I);
                Ind_Trie (I)       := Ind_Trie (Ind_Max);
                Ind_Trie (Ind_Max) := Temp_Ind;
            end loop;
        end Tri_Insertion;

        N                           : constant Integer := V'Length;
        V_Trie                      : T_Vecteur        := V;
        Ind_Trie                    : T_Vecteur_Ind (0 .. N - 1);
        Fichier_Poids, Fichier_Rang : File_Type;
    begin

        -- Tri des poids par ordre décroissant et N° de noeuds associés
        Tri_Insertion (V_Trie, Ind_Trie);

        -- Écriture des N° de noeuds
        Create (Fichier_Rang, Out_File, Nom_Fichier & ".pr");
        for I in 0 .. N - 1 loop
            Put (Fichier_Rang, Ind_Trie (I), Width => 0);
            New_Line (Fichier_Rang);
        end loop;
        Close (Fichier_Rang);

        -- Écriture des poids triés
        Create (Fichier_Poids, Out_File, Nom_Fichier & ".prw");
        Put (Fichier_Poids, N, Width => 0);
        Put (Fichier_Poids, " ");
        Put (Fichier_Poids, Alpha, Fore => 1, Exp => 0);
        Put (Fichier_Poids, " ");
        Put (Fichier_Poids, K, Width => 0);
        for I in 0 .. N - 1 loop
            New_Line (Fichier_Poids);
            Put (Fichier_Poids, Item => V_Trie (I), Fore => 1, Exp => 0);
        end loop;
        Close (Fichier_Poids);

    end Ecrire;

    -- instanciation des procédures génériques d'écriture
    procedure Ecrire_Creux is new Ecrire (PR_Matrice_Creuse.T_Vecteur);
    procedure Ecrire_Plein is new Ecrire (PR_Matrice_Pleine.T_Vecteur);

    -- Nom du fichier est dernier des arguments
    Nom_Fichier : Unbounded_String;
    Fichier     : File_Type;

    Ligne : Unbounded_String;
    N     : Integer;
    Dummy : Integer;

begin
    -- si l'utilisateur veut obtenir la documentation
    if (Argument_Count = 0) or else (Argument (1) = "-H") then
        raise Afficher_Doc_Exception;
    end if;

    Get_Args (Nom_Fichier);

    -- extraction de N : nombre de noeuds
    begin
        Open
           (File => Fichier, Mode => In_File, Name => To_String (Nom_Fichier));
        Ligne := To_Unbounded_String (Get_Line (Fichier));
        Close (Fichier);
        Get (To_String (Ligne), N, Dummy);
        if (N <= 0) then
            raise Fichier_Format_Exception;
        end if;

    exception

        when Ada.IO_Exceptions.End_Error =>
            raise Fichier_Vide_Exception
               with "Le fichier """ & To_String (Nom_Fichier) &
               """ est vide. ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";

        when Ada.IO_Exceptions.Name_Error => -- le fichier n'existe pas
            raise Fichier_Introuvable_Exception
               with "Le fichier """ & To_String (Nom_Fichier) &
               """ est introuvable. Vérifiez son chemin d'accès. ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";

        when Data_Error
           | Fichier_Format_Exception => -- pas d'entier trouvé, ou l'entier n'est pas positif strictement
            raise Fichier_Format_Exception
               with "Le fichier """ & To_String (Nom_Fichier) &
               """ est du mauvais format. ""./pagerank -H"" vous affichera la documentation complète de l'application PageRank.";
    end;

    if C_Val then

        -- * Implémentation matrice creuse

        -- Calcule puis écrit le vecteur trié des poids
        Ecrire_Creux
           (Calcul_Creux (To_String (Nom_Fichier), N, K_Val, A_Val, E_Val),
            To_String (R_Val), A_Val, K_Val);

    else

        -- * Implémentation matrice pleine

        -- Calcule puis écrit le vecteur trié des poids
        Ecrire_Plein
           (Calcul_Plein (To_String (Nom_Fichier), N, K_Val, A_Val, E_Val),
            To_String (R_Val), A_Val, K_Val);

    end if;

exception -- traitement de l'exception dans le bloc parent de tous pour stoper l'exécution du programme
    when Afficher_Doc_Exception =>
        Afficher_Doc;

end PageRank;
