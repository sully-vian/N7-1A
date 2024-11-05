with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with SDA_Exceptions;        use SDA_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with LCA;

procedure LCA_Sujet is

    package LCA_String_Integer is new LCA
       (T_Cle => Unbounded_String, T_Valeur => Integer);
    use LCA_String_Integer;

    procedure Put_Unbounded (S : in Unbounded_String) is
    begin
        Put (To_String (S), 1);
    end Put_Unbounded;

    procedure Put_Integer (N : in Integer) is
    begin
        Put (N, 1);
    end Put_Integer;

    --  procedure Afficher_Interne is new Afficher_Debug
    --     (Afficher_Cle => Put_Unbounded, Afficher_Donnee => Put_Integer);

    procedure Traiter_Afficher (Cle : in Unbounded_String; Valeur : in Integer)
    is
    begin
        Put_Unbounded (Cle);
        Put (" : ");
        Put_Interger
        New_Line;
    end Traiter_Afficher;

    procedure Afficher is new Pour_Chaque (Traiter => Traiter_Afficher);

    Sda  : T_LCA;
    Un   : constant Unbounded_String := To_Unbounded_String ("un");
    Deux : constant Unbounded_String := To_Unbounded_String ("deux");
begin
    Initialiser (Sda);
    Enregistrer (Sda, Un, 1);
    Enregistrer (Sda, Deux, 2);
    Afficher (Sda);
end LCA_Sujet;
