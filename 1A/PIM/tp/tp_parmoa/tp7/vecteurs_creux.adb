with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body Vecteurs_Creux is

    procedure Free is new Ada.Unchecked_Deallocation
       (T_Cellule, T_Vecteur_Creux);

    procedure Initialiser (V : out T_Vecteur_Creux) is
    begin
        V := null;
    end Initialiser;

    procedure Detruire (V : in out T_Vecteur_Creux) is
    begin
        if V = null then
            null;
        else
            Detruire (V.Suivant);
            Free (V);
        end if;
    end Detruire;

    function Est_Nul
       (V : in T_Vecteur_Creux; Epsilon : in Float) return Boolean
    is
    begin
        if V = null then
            return True;
        else
            if (V.Valeur * V.Valeur < Epsilon) then
                return Est_Nul (V.Suivant, Epsilon);
            else
                return False;
            end if;
        end if;
    end Est_Nul;

    function Composante_Recursif
       (V : in T_Vecteur_Creux; Indice : in Integer) return Float
    is
    begin
        if (Indice = V.Indice) then
            return V.Valeur;
        else
            return Composante_Recursif (V.Suivant, Indice);
        end if;
    end Composante_Recursif;

    function Composante_Iteratif
       (V : in T_Vecteur_Creux; Indice : in Integer) return Float
    is
        Vect : T_Vecteur_Creux := V;
    begin
        while (Indice /= Vect.Indice) loop
            Vect := Vect.Suivant;
        end loop;
        return Vect.Valeur;
    end Composante_Iteratif;

    procedure Modifier
       (V : in out T_Vecteur_Creux; Indice : in Integer; Valeur : in Float)
    is
    begin
        if (V = null) then
            null;
        elsif V.Indice = Indice then
            V.Valeur := Valeur;
        else
            Modifier (V.Suivant, Indice, Valeur);
        end if;
    end Modifier;

    function Sont_Egaux_Recursif
       (V1, V2 : in T_Vecteur_Creux; Epsilon : in Float) return Boolean
    is
    begin
        if (V1 = null and V2 = null) then
            return True;
        elsif ((V1.Valeur - V2.Valeur) * (V1.Valeur - V2.Valeur) < Epsilon)
        then
            return Sont_Egaux_Recursif (V1.Suivant, V2.Suivant, Epsilon);
        else
            return False;
        end if;
    end Sont_Egaux_Recursif;

    function Sont_Egaux_Iteratif
       (V1, V2 : in T_Vecteur_Creux; Epsilon : in Float) return Boolean
    is
        Vect1 : T_Vecteur_Creux := V1;
        Vect2 : T_Vecteur_Creux := V2;
    begin
        while (Vect1 /= null and Vect2 /= null) loop
            if ((Vect1.Valeur - Vect2.Valeur) * (Vect1.Valeur - Vect2.Valeur) <
                Epsilon)
            then
                Vect1 := Vect1.Suivant;
                Vect2 := Vect2.Suivant;
            else
                return False;
            end if;
        end loop;
        return True;
    end Sont_Egaux_Iteratif;

    procedure Additionner
       (V1 : in out T_Vecteur_Creux; V2 : in T_Vecteur_Creux)
    is
    begin
        if (V1 = null) then
            null;
        else
            V1.Valeur := V1.Valeur + V2.Valeur;
            Additionner (V1.Suivant, V2.Suivant);
        end if;
    end Additionner;

    function Norme2 (V : in T_Vecteur_Creux) return Float is
    begin
        if (V = null) then
            return 0.0;
        else
            return (V.Valeur * V.Valeur) + Norme2 (V.Suivant);
        end if;
    end Norme2;

    function Produit_Scalaire (V1, V2 : in T_Vecteur_Creux) return Float is
    begin
        if (V1 = null) then
            return 0.0;
        else
            return
               (V1.Valeur * V2.Valeur) +
               Produit_Scalaire (V1.Suivant, V2.Suivant);
        end if;
    end Produit_Scalaire;

    procedure Afficher (V : T_Vecteur_Creux) is
    begin
        if V = null then
            Put ("--E");
        else
            -- Afficher la composante V.all
            Put ("-->[ ");
            Put (V.Indice, 0);
            Put (" | ");
            Put (V.Valeur, Fore => 0, Aft => 1, Exp => 0);
            Put (" ]");

            -- Afficher les autres composantes
            Afficher (V.Suivant);
        end if;
    end Afficher;

    function Nombre_Composantes_Non_Nulles
       (V : in T_Vecteur_Creux) return Integer
    is
    begin
        if V = null then
            return 0;
        else
            return 1 + Nombre_Composantes_Non_Nulles (V.Suivant);
        end if;
    end Nombre_Composantes_Non_Nulles;

end Vecteurs_Creux;
