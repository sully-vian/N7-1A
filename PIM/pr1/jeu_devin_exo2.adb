with Text_Io;              use Text_Io;
with Ada.Integer_Text_Io;  use Ada.Integer_Text_Io;
with Alea;

-- Auteur : Vianney HERVY, Groupe H2

-- R0 : Faire jouer le jeu du devin à l'ordinateur

-- Exemple :

-- L’utilisateur choisit 187

--      Sorties de l’ordinateur      |  Entrées de l’utilisateur
-- - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - -
-- Je pense que votre nombre est 500 |          ‘g’
-- Je pense que votre nombre est 250 |          ‘g’
-- Je pense que votre nombre est 125 |          ‘p’
-- Je pense que votre nombre est 187 |          ‘t’

-- R1 : Comment faire jouer le jeu du devin à l’ordinateur ?
procedure Jeu_Devin_Exo2 is
    -- définition des variables
    Borne_Min: Integer; -- borne min
    Borne_Max: Integer; -- borne max
    i: Integer; -- indice des propositions
    n: Integer; -- proposition de l'ordinateur
    reponse: Character; -- indication de l'utilisateur
    nombre_trouve: Boolean; -- indique si le nombre a été trouvé
    triche: Boolean; -- indique si une triche est détectée
    
    begin
    -- 	Initialisation des variables
    Borne_Min := 1;
    Borne_Max := 999;
    i := 1;
    
    -- Réalisation du jeu
    loop
        -- Choix intelligent du nombre
        n := (Borne_Min + Borne_Max) / 2; -- la recherche par dichotomie impose le choix du nombre au “milieu” de l’intervalle.
        
        -- Proposition du nombre à l’utilisateur
        Put_Line("Proposition" & Integer'Image(i) & " :" & Integer'Image(n));
        
        -- Demande de sa réponse à l’utilisateur
        Put_Line("Trop (g)rand, trop (p)etit ou (t)rouvé ?");
        Put_Line(Integer'Image(Borne_Min) & Integer'Image(Borne_Max));
        Get (reponse);
        Skip_Line;
        
        -- Traitement de la réponse de l’utilisateur
        case reponse is
            when 'p'|'P' => Borne_Min := n;
                            i := i + 1;
                            if (Borne_Min + 1) = Borne_Max then
                                triche := True;
                            end if;
            when 'g'|'G' => Borne_Max := n;
                            i := i + 1;
            when 't'|'T' => nombre_trouve := True;
            when others => null;
        end case;

        -- Vérification de si l’utilisateur triche
        if not(triche) then
            triche := Borne_Max = Borne_Min;
        end if;
    exit when nombre_trouve or triche;
    end loop;

    -- 	Affichage (ou non) de la triche
    if triche then
        Put ("Vous trichez. J'arrête cette partie.");
    end if;
    
    -- 	Affichage (ou non) de la victoire
    if nombre_trouve then
        Put_Line("J'ai trouvé" & Integer'Image(n) & " en" & Integer'Image(i) & " essais(s).");
    end if;

end Jeu_Devin_Exo2;
