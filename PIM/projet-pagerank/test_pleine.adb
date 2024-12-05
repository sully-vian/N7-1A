with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Matrice_Pleine;

procedure Test_Pleine is

    package Matrice_Pleine_Float is new Matrice_Pleine (Float);
    use Matrice_Pleine_Float;

    -- égalité de deux vecteurs
    function "=" (A, B : T_Vecteur) return Boolean is
        Resultat : Boolean := True;
    begin
        if (A'Length /= B'Length) then
            Resultat := False;
        else
            for I in A'Range loop
                if A (I) /= B (I) then
                    Resultat := False;
                end if;
            end loop;
        end if;
        return Resultat;
    end "=";

    -- égalité de deux matrices
    function "=" (A, B : T_Matrice) return Boolean is
        Resultat : Boolean := True;
    begin
        if A'Length (1) /= B'Length (1) or else A'Length (2) /= B'Length (2)
        then
            Resultat := False;
        else
            for I in A'Range (1) loop
                for J in A'Range (2) loop
                    if A (I, J) /= B (I, J) then
                        Resultat := False;
                    end if;
                end loop;
            end loop;
        end if;
        return Resultat;
    end "=";

    procedure Test_Norme2 is
        V1 : constant T_Vecteur := (1.0, 2.0, 2.0);
        V2 : constant T_Vecteur := (3.0, 4.0, 0.0);
        V3 : constant T_Vecteur := (0.0, 0.0, 0.0);
    begin
        pragma Assert (Norme2 (V1) = 9.0);
        pragma Assert (Norme2 (V2) = 25.0);
        pragma Assert (Norme2 (V3) = 0.0);
        Put_Line ("Test_Norme2 : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Norme2");
    end Test_Norme2;

    procedure Test_Soustraction_Vecteur is
        A : constant array (0 .. 1) of T_Vecteur (0 .. 2) :=
           ((1.0, 2.0, 3.0), (5.0, 6.0, 7.0));
        B : constant array (0 .. 1) of T_Vecteur (0 .. 2) :=
           ((4.0, 5.0, 6.0), (1.0, 2.0, 3.0));
        C : constant array (0 .. 1) of T_Vecteur (0 .. 2) :=
           ((-3.0, -3.0, -3.0), (4.0, 4.0, 4.0));
    begin
        for I in A'Range loop
            pragma Assert (A (I) - B (I) = C (I));
        end loop;
        Put_Line ("Test_Soustraction_Vecteur : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Soustraction_Vecteur");
    end Test_Soustraction_Vecteur;

    procedure Test_Addition_Matrice is
        A : constant array (0 .. 1) of T_Matrice (0 .. 1, 0 .. 1) :=
           (((1.0, 2.0), (3.0, 4.0)), ((-1.0, -2.0), (-3.0, -4.0)));
        B : constant array (0 .. 1) of T_Matrice (0 .. 1, 0 .. 1) :=
           (((5.0, 6.0), (7.0, 8.0)), ((1.0, 2.0), (3.0, 4.0)));
        C : constant array (0 .. 1) of T_Matrice (0 .. 1, 0 .. 1) :=
           (((6.0, 8.0), (10.0, 12.0)), ((0.0, 0.0), (0.0, 0.0)));
    begin
        for I in A'Range loop
            pragma Assert (A (I) + B (I) = C (I));
        end loop;
        Put_Line ("Test_Addition_Matrice : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Addition_Matrice");
    end Test_Addition_Matrice;

    procedure Test_Produit_Matriciel is
        A : constant T_Matrice (0 .. 1, 0 .. 1) := ((1.0, 2.0), (3.0, 4.0));
        B : constant T_Matrice (0 .. 1, 0 .. 1) := ((5.0, 6.0), (7.0, 8.0));
        C : constant T_Matrice (0 .. 1, 0 .. 1) :=
           ((19.0, 22.0), (43.0, 50.0));
    begin
        pragma Assert (A * B = C);
        Put_Line ("Test_Produit_Matriciel : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Produit_Matriciel");
    end Test_Produit_Matriciel;

    procedure Test_Multiplication_Scalaire_Matrice is
        A : constant T_Matrice (0 .. 1, 0 .. 1) := ((1.0, 2.0), (3.0, 4.0));
        X : constant array (0 .. 2) of Float := (2.0, 0.0, -1.0);
        B : constant array (0 .. 2) of T_Matrice (0 .. 1, 0 .. 1) :=
           (((2.0, 4.0), (6.0, 8.0)), ((0.0, 0.0), (0.0, 0.0)),
            ((-1.0, -2.0), (-3.0, -4.0)));
    begin
        for I in X'Range loop
            pragma Assert (X (I) * A = B (I));
        end loop;
        Put_Line ("Test_Multiplication_Scalaire_Matrice : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Multiplication_Scalaire_Matrice");
    end Test_Multiplication_Scalaire_Matrice;

    procedure Test_Exponentiation_Rapide is
        A : constant array (0 .. 2) of T_Matrice (0 .. 1, 0 .. 1)         :=
           (((1.0, 0.0), (0.0, 1.0)), ((1.0, 2.0), (3.0, 4.0)),
            ((-3.0, -4.0), (-1.0, -2.0)));
        B : constant array (0 .. 2, 0 .. 2) of T_Matrice (0 .. 1, 0 .. 1) :=
           ((((1.0, 0.0), (0.0, 1.0)), ((1.0, 0.0), (0.0, 1.0)),
             ((1.0, 0.0), (0.0, 1.0))),
            (((165_751.0, 241_570.0), (362_355.0, 528_106.0)),
             ((30_853.0, 44_966.0), (67_449.0, 98_302.0)),
             ((37.0, 54.0), (81.0, 118.0))),
            (((116_461.0, 181_860.0), (45_465.0, 70_996.0)),
             ((-25_531.0, -39_868.0), (-9_967.0, -15_564.0)),
             ((-59.0, -92.0), (-23.0, -36.0))));
        K : constant array (0 .. 2) of Integer := (8, 7, 3);
        C : T_Matrice (0 .. 1, 0 .. 1);
    begin
        for I in 0 .. 2 loop
            for J in 0 .. 2 loop
                C := A (I);
                Exponentiation_Rapide (C, K (J));
                pragma Assert (C = B (I, J));
            end loop;
        end loop;
        Put_Line ("Test_Exponentiation_Rapide : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Exponentiation_Rapide");
    end Test_Exponentiation_Rapide;

    procedure Test_Multiplication_Vecteur_Matrice is
        A : constant T_Matrice (0 .. 1, 0 .. 1) := ((1.0, 2.0), (3.0, 4.0));
        X : constant array (0 .. 2) of T_Vecteur (0 .. 1) :=
           ((1.0, 2.0), (3.0, 4.0), (5.0, 6.0));
        B : constant array (0 .. 2) of T_Vecteur (0 .. 1) :=
           ((7.0, 10.0), (15.0, 22.0), (23.0, 34.0));
    begin
        for I in X'Range loop
            pragma Assert (X (I) * A = B (I));
        end loop;
        Put_Line ("Test_Multiplication_Vecteur_Matrice : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Multiplication_Vecteur_Matrice");
    end Test_Multiplication_Vecteur_Matrice;

    procedure Test_Extraire_Arc is
        N       : constant array (0 .. 3) of Integer := (3, 5, 57, 435_443);
        Arcs    : constant array (0 .. 3) of Unbounded_String :=
           (To_Unbounded_String ("1 2"), To_Unbounded_String ("3 4"),
            To_Unbounded_String ("56 45"),
            To_Unbounded_String ("435_442 36_552"));
        Sommets : constant array (0 .. 3, 0 .. 1) of Integer  :=
           ((1, 2), (3, 4), (56, 45), (435_442, 36_552));
        S1, S2  : Integer;
    begin
        for I in Arcs'Range loop
            Extraire_Arc (To_String (Arcs (I)), S1, S2, N (I));
            pragma Assert (S1 = Sommets (I, 0));
            pragma Assert (S2 = Sommets (I, 1));
        end loop;
        Put_Line ("Test_Extraire_Arc : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Extraire_Arc");
    end Test_Extraire_Arc;

begin

    Put_Line ("----------| Test Matrice_Pleine |----------");

    New_Line;
    Test_Norme2;

    New_Line;
    Test_Soustraction_Vecteur;

    New_Line;
    Test_Addition_Matrice;

    New_Line;
    Test_Produit_Matriciel;

    New_Line;
    Test_Multiplication_Scalaire_Matrice;

    New_Line;
    Test_Exponentiation_Rapide;

    New_Line;
    Test_Multiplication_Vecteur_Matrice;

    New_Line;
    Test_Extraire_Arc;

    New_Line;
    Put_Line ("----------| Fin Test Matrice_Pleine |----------");

end Test_Pleine;
