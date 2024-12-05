with Text_Io;              use Text_Io;
with Ada.Integer_Text_Io;  use Ada.Integer_Text_Io;
with Alea;

-- Auteur : Emmanuel DUBOIS, Groupe H2
--
-- R0 : Faire deviner un nombre à l'utilisateur
--
-- Exemple de déroulement d'une partie
-- Nombre choisi par l'ordinateur : 967
--
-- Entrées de l'utilisateur      |       Sorties de l'ordinateur
-- - - - - - - - - - - - - - - - | - - - - - - - - - - - - - - - - -
--                               |  J'ai choisi un nombre entre 1 et 999.
--                               |  Proposition 1 :
--              900              |  Trop petit.
--                               |  Proposition 2 :
--              990              |  Trop grand.
--                               |  Proposition 3 :
--              960              |  Trop petit.
--                               |  Proposition 4 :
--              967              |  Trouvé.
--                               |  Bravo. Vous avez trouvé 967 en 4 essais.


-- R1 : Comment faire deviner un nombre à l'utilisateur
procedure Jeu_Devin_Exo1 is

    -- Définition des variables

    Nombre : Integer; -- Nombre choisi par l'ordinateur
    C : Integer; -- Compteur du nombre d'essais que l'utilisateur fait pour deviner le nombre
    Dev : Integer; -- Valeur entrée par l'utilisateur 
    package Mon_Alea is
        new Alea(1,999); -- Permet à l'ordinateur de choisir un nombre compris dans les bornes
    use Mon_Alea;

begin

    -- Initialisation des variables

    Get_Random_Number(Nombre); -- Faire choisir un nombre à l'ordinateur
    Put_Line("J'ai choisi un nombre compris entre 1 et 999.");
    C := 0; -- Initialisation du compteur
    Dev := 0;

    -- Début de la partie

    while Dev /= Nombre loop -- faire deviner le nombre à l'utilisateur
        C := C + 1; -- Incrémentation du compteur
        Put("Proposition " & Integer'Image(C) & " : "); -- Demander un nombre à l'utilisateur
        Get(Dev);

        -- Vérification de l'essai de l'utilisateur

        if Dev = Nombre then -- Fin de partie
            Put_Line("Trouvé.");
            Put_Line("Bravo. Vous avez trouvé" & Integer'Image(Nombre) & " en" & Integer'Image(C) & " essais.");

        -- Cas où l'utilisateur n'a pas deviné
        
        elsif Dev > Nombre then
            Put_Line("Trop grand.");
        else
            Put_Line("Trop petit.");
        end if;
    end loop;
end Jeu_Devin_Exo1;


