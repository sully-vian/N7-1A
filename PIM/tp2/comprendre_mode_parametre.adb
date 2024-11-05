with Ada.Text_IO;
use Ada.Text_IO;

-- Dans ce programme, les commentaires de spécification
-- ont **volontairement** été omis !

procedure Comprendre_Mode_Parametre is

    function Double (N : in Integer) return Integer is
    begin
        return 2 * N;
    end Double;

    procedure Incrementer (N : in out Integer) is
    begin
        N := N + 1;
    end Incrementer;

    procedure Mettre_A_Zero (N : out Integer) is
    begin
        N := 0;
    end Mettre_A_Zero;

    procedure Comprendre_Les_Contraintes_Sur_L_Appelant is
        A, B, R : Integer;
    begin
        A := 5;
        -- Indiquer pour chacune des instructions suivantes si elles sont
        -- acceptées par le compilateur.
        R := Double (A); -- Oui
        R := Double (10); -- Oui
        R := Double (10 * A); -- Oui
        R := Double (B); -- Oui par le compilateur (mais B pas init)

        Incrementer (A); -- Oui
        Incrementer (10); -- Non (10 pas une variable)
        Incrementer (10 * A); -- Non (10*A pas une variable)
        Incrementer (B); -- Oui par le compilateur (mais B pas init)

        Mettre_A_Zero (A); -- Oui
        Mettre_A_Zero (10); -- Non (10 pas une variable)
        Mettre_A_Zero (10 * A); -- Non (10*A pas une variable)
        Mettre_A_Zero (B); -- Oui (Initialise B à 0)
    end Comprendre_Les_Contraintes_Sur_L_Appelant;


    procedure Comprendre_Les_Contrainte_Dans_Le_Corps (
            A      : in Integer;
            B1, B2 : in out Integer;
            C1, C2 : out Integer)
    is
        L: Integer;
    begin
        -- pour chaque affectation suivante indiquer si elle est autorisée
        L := A; -- Oui
        A := 1; -- Non (A en in, on change pas sa valeur)

        B1 := 5; -- Oui

        L := B2; -- Oui
        B2 := B2 + 1; -- Oui

        C1 := L; -- Oui

        L := C2; -- Oui (même si C2 en out ?)

        C2 := A; -- Oui
        C2 := C2 + 1; -- Oui (même si C2 en out ?)
    end Comprendre_Les_Contrainte_Dans_Le_Corps;


begin
    Comprendre_Les_Contraintes_Sur_L_Appelant;
    Put_Line ("Fin");
end Comprendre_Mode_Parametre;