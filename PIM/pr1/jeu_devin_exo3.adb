with Text_Io;              use Text_Io;
with Ada.Integer_Text_Io;  use Ada.Integer_Text_Io;
with Alea;
with Jeu_Devin_Exo1;
with Jeu_Devin_Exo2;

-- Auteur : Emmanuel DUBOIS, Groupe H2 
--
-- R0 : Jouer avec l'ordinateur au jeu du devin
--
-- Exemple d'exécution :

--              Sorties de l’ordinateur              |  Entrées de l’utilisateur
-- - - - - - - - - - - - - - - - - - - - - - - - - - | - - - - - - - - - - - - -
-- Voulez-vous choisir ou deviner un nombre [c/d] ?  |          ‘c’
-- Je pense que votre nombre est 500                 |          ‘g’
-- Je pense que votre nombre est 250                 |          ‘g’
-- Je pense que votre nombre est 125                 |          ‘p’
-- Je pense que votre nombre est 187                 |          ‘t’
-- Voulez-vous choisir ou deviner un nombre [c/d] ?  |          ‘d’
-- J'ai choisi un nombre entre 1 et 999.             |          900
--                    ‘Trop petit’                   |          990
--                    ‘Trop grand’                   |          932
--                    ‘Trop petit’                   |          940
--                     ‘Trouvé !’                    |


-- R1 : Comment jouer avec l'ordinateur au jeu du devin ?

procedure Jeu_Devin_Exo3 is

        -- Définition des variables

        Entree : Integer; -- Variable stockant le choix de l'utilisateur concernant le type de partie jouée
begin
        Entree := 1;

        -- Boucle de jeu

        while Entree /= 0 loop

                -- Affichage des consignes

                Put_Line("1 - L'ordinateur choisit un nombre et vous le devinez");
                Put_Line("2 - Vous choisissez un nombre et l'ordinateur le devine");
                Put_Line("0 - Quitter le programme");

                -- Réception et traitement de la commande utilisateur
                
                Put("Votre choix : ");
                Get(Entree);
                while ((Entree < 0) or (Entree > 2)) loop
                        Put_Line("Choix incorrect.");
                        Put("Votre choix : ");
                        Get(Entree);
                end loop;
	        if entree = 1 then
	                Jeu_Devin_Exo1;
	        elsif entree = 2 then
                        Jeu_Devin_Exo2;
                else
                        Put_Line("Au revoir...");
                end if;
        end loop;
end Jeu_Devin_Exo3;
