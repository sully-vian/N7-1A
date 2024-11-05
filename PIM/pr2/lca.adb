with Ada.Text_IO;    use Ada.Text_IO;
with SDA_Exceptions; use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

    procedure Free is new Ada.Unchecked_Deallocation
       (Object => T_Cellule, Name => T_LCA);

    procedure Initialiser (Sda : out T_LCA) is
    begin
        Sda := null;
    end Initialiser;

    procedure Detruire (Sda : in out T_LCA) is
    begin
        if (Sda /= null) then
            Detruire (Sda.Suivant);
            Free (Sda);
        end if;
    end Detruire;

    procedure Enregistrer
       (Sda : in out T_LCA; Cle : in T_Cle; Valeur : in T_Valeur)
    is
    begin
        if (Sda = null) then
            Sda := new T_Cellule'(Cle, Valeur, null);
        elsif (Sda.Cle = Cle) then
            Sda.Valeur := Valeur;
        else
            Enregistrer (Sda.Suivant, Cle, Valeur);
        end if;
    end Enregistrer;

    procedure Supprimer (Sda : in out T_LCA; Cle : in T_Cle) is
        Temp : T_LCA;
    begin
        if (Sda = null) then
            raise Cle_Absente_Exception;
        elsif (Sda.Cle = Cle) then
            Temp := Sda.Suivant.Suivant;
            Free (Sda);
            Sda.Suivant := Temp;
        else
            Supprimer (Sda.Suivant, Cle);
        end if;
    end Supprimer;

    function Cle_Presente (Sda : in T_LCA; Cle : in T_Cle) return Boolean is
    begin
        if (Sda = null) then
            return False;
        else
            return (Sda.Cle = Cle) or else Cle_Presente (Sda.Suivant, Cle);
        end if;
    end Cle_Presente;

    function La_Valeur (Sda : in T_LCA; Cle : in T_Cle) return T_Valeur is
    begin
        if (Sda = null) then
            raise Cle_Absente_Exception;
        elsif (Sda.Cle = Cle) then
            return Sda.Valeur;
        else
            return La_Valeur (Sda.Suivant, Cle);
        end if;
    end La_Valeur;

    function Taille (Sda : in T_LCA) return Integer is
    begin
        if (Sda = null) then
            return 0;
        else
            return 1 + Taille (Sda.Suivant);
        end if;
    end Taille;

    procedure Afficher_Debug (Sda : in T_LCA) is
    begin
        if (Sda = null) then
            Put ("--E");
        else
            Put ("-->[");
            Afficher_Cle (Sda.Cle);
            Put (" : ");
            Afficher_Donnee (Sda.Valeur);
            Put ("]");
            Afficher_Debug (Sda.Suivant);
        end if;
    end Afficher_Debug;

    procedure Pour_Chaque (Sda : in T_LCA) is
    begin
        if (Sda /= null) then
            Traiter (Sda.Cle, Sda.Valeur);
            Pour_Chaque (Sda.Suivant);
        end if;
    end Pour_Chaque;

    function Est_Vide (Sda : T_LCA) return Boolean is
    begin
        return (Sda = null);
    end Est_Vide;

end LCA;
