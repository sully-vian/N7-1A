with Ada.Text_IO; use Ada.Text_IO;

procedure Dates is

    function Est_Bissextile(Annee: in Integer) return Boolean with
            Pre => True,
            Post => True
    is
    begin
        return (Annee mod 4 = 0);
    end Est_Bissextile;

    function Nombre_Jours(Mois: in Integer ; Annee: in Integer) return Integer with
            Pre => (Mois <= 12) and (Mois >= 1),
            Post => (Nombre_Jours'Result <= 31)
                and (Nombre_Jours'Result >= 28)
    is
        Nb_Jours: Integer;
    begin
        case Mois is
            when 1|3|5|7|8|10|12 => Nb_Jours := 31;
            when 4|6|9|11 => Nb_Jours := 30;
            when 2 =>
                if Est_Bissextile(Annee) then
                    Nb_Jours := 29;
                else
                    Nb_Jours := 28;
                end if;
            when others => null;
        end case;
        return Nb_Jours;
    end Nombre_Jours;

    procedure Tester_Nombre_Jours is
    begin
        pragma Assert (Nombre_Jours(1, 2023) = 31);
        pragma Assert (Nombre_Jours(7, 2023) = 31);
        pragma Assert (Nombre_Jours(4, 2023) = 30);
        pragma Assert (Nombre_Jours(2, 2023) = 28);
        pragma Assert (Nombre_Jours(2, 2024) = 29);
    end Tester_Nombre_Jours;
    -- Modifier les valeurs données en entrée pour qu'elles soient celles du lendemain
    -- Paramètres :
    -- Jour: in out Integer
    -- Mois: in out Integer
    -- Annee: in out Integer
    -- Exemples : voir Tester_Modifier_date
    procedure Modifier_Date(Jour: in out Integer ; Mois: in out Integer ; Annee: in out Integer) with
            Pre => (Jour >= 0 and Jour <= Nombre_Jours(Mois, Annee))
               and (Mois >= 0 and Mois <= 12),
            Post => (Jour >= 0 and Jour <= Nombre_Jours(Mois, Annee))
                and (Mois >= 0 and Mois <= 12)
    is
    begin
        if Jour < Nombre_Jours(Mois, Annee) then
            Jour := Jour + 1;
        elsif Mois < 12 then
            Jour := 1;
            Mois := Mois + 1;
        else
            Jour := 1;
            Mois := 1;
            Annee := Annee + 1;
        end if;
    end Modifier_Date;

    procedure Tester_Modifier_Date is
        Jour, Mois, Annee: Integer;
    begin
        --- Test 1 : 4/4/2003 -> 5/4/3/2003
        Jour := 4;
        Mois := 4;
        Annee := 2003;
        Modifier_Date (Jour, Mois, Annee);
        pragma Assert ((Jour = 5) and (Mois = 4) and (Annee = 2003));

        --- Test 2 : 30/9/2023 -> 1/10/2023
        Jour := 30;
        Mois := 9;
        Annee := 2023;
        Modifier_Date (Jour, Mois, Annee);
        pragma Assert (Jour = 1 and Mois = 10 and Annee = 2023);

        --- Test 2 : 31/12/2023 -> 1/1/2024
        Jour := 31;
        Mois := 12;
        Annee := 2023;
        Modifier_Date (Jour, Mois, Annee);
        pragma Assert (Jour = 1 and Mois = 1 and Annee = 2024);
    end Tester_Modifier_Date;

begin
    Tester_Nombre_Jours;
    Tester_Modifier_Date;
    Put_Line("Terminadoooooooo");

end Dates;