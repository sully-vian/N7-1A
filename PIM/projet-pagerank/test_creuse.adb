with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Matrice_Creuse;

procedure Test_Creuse is
    -- instanciation de Matrice_Pleine
    package Float_Matrice_Creuse is new Matrice_Creuse (Float);
    use Float_Matrice_Creuse;
    package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
    use Int_IO;

    procedure Test_Norme2Diff is
        V1 : constant T_Vecteur := (1.0, 2.0, 2.0);
        V2 : constant T_Vecteur := (3.0, 4.0, 0.0);
        V3 : constant T_Vecteur := (0.0, 0.0, 0.0);
    begin
        pragma Assert (Norme2Diff (V1, V2) - 12.0 < 0.01);
        pragma Assert (Norme2Diff (V1, V3) - 9.0 < 0.01);
        pragma Assert (Norme2Diff (V3, V2) - 25.0 < 0.01);

        Put_Line ("Test_Norme2Diff : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Test_Norme2Diff");
    end Test_Norme2Diff;

    procedure Verif_produit_affine
       (V : in out T_Vecteur; M : in out T_Matrice_Creuse; N : in Integer)
    is
    begin
        Initialiser (M (0));
        Initialiser (M (1));
        Initialiser (M (2));
        Modifier (M (0), 1, 2.0);
        Modifier (M (1), 0, 2.0);
        Modifier (M (1), 2, 3.0);
        Modifier (M (2), 2, 5.0);
        V := produit (V, M, 0.0, 3);
        pragma Assert (V (0) - 3.0 < 0.01);
        pragma Assert (V (1) - 6.0 < 0.01);
        pragma Assert (V (2) - 6.0 < 0.01);

        affine_creux (M, 2.0, 3);
        pragma Assert (Composante_Recursif (M (0), 1) - 4.0 < 0.01);
        pragma Assert (Composante_Recursif (M (1), 2) - 6.0 < 0.01);
        pragma Assert (Composante_Recursif (M (2), 2) - 10.0 < 0.01);

        Put_Line ("Verif_produit_affine : OK");
    exception
        when others =>
            Put_Line ("Erreur dans Verif_produit_affine");

    end Verif_produit_affine;

    V : T_Vecteur (0 .. 2) := (1.0, 1.0, 1.0);
    M : T_Matrice_Creuse (0 .. 2);
begin

    Put_Line ("----------| Test Matrice_Creuse |----------");

    New_Line;
    Test_Norme2Diff;

    New_Line;
    Verif_produit_affine (V, M, 3);

    New_Line;
    Put_Line ("----------| Fin Test Matrice_Creuse |----------");

end Test_Creuse;
