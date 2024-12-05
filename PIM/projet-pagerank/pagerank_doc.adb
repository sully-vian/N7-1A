with Ada.Text_IO; use Ada.Text_IO;

package body PageRank_Doc is

    -- Écrire avec indentation
    procedure Put_Indent (S : String) is
    begin
        Set_Col (10);
        Put_Line (S);
    end Put_Indent;

    procedure Afficher_Generalites is
    begin
        New_Line;
        Put_Indent ("----------| GÉNÉRALITÉS |----------");
        New_Line;
        Put_Line
           ("Ce programme permet de calculer la ""popularité"" de nœuds dans un graphe");
        Put_Line ("orienté à l'aide de l'algorithme de PageRank.");
        New_Line;
        Put_Line ("Utilisation :");
        Put_Indent
           ("> ./pagerank [-A <valeur>] [-K <valeur>] [-E <valeur>] [-P] [-C] [-R <prefixe>] sujet.net");
    end Afficher_Generalites;

    procedure Afficher_A_Doc is
    begin
        Put_Line ("-A <valeur>     : Float     (0.0 <= A <= 1.0))");
        Put_Indent
           ("Définir la valeur de alpha. La valeur doit être comprise entre 0 et 1 au sens");
        Put_Indent
           ("large. Si l'option -A n'est pas précisée, on utilisera 0.85.");
    end Afficher_A_Doc;

    procedure Afficher_K_Doc is
    begin
        Put_Line ("-K <valeur>     : Integer   (K >= 0)");
        Put_Indent
           ("Définir le nombre maximal d'itérations à effectuer pour calculer le vecteur.");
        Put_Indent
           ("La valeur doit être un entier positif. Sa valeur par défaut est 150.");
    end Afficher_K_Doc;

    procedure Afficher_E_Doc is
    begin
        Put_Line ("-E <valeur>     : Float     (E >= 0.0)");
        Put_Indent
           ("Définir une précision (un epsilon) qui permettra d'interrompre le calcul du");
        Put_Indent
           ("PageRank si le vecteur poids est à une distance du vecteur poids précédent");
        Put_Indent
           ("strictement inféreure à epsilon. La valeur est un nombre réel positif. Sa valeur");
        Put_Indent
           ("par défaut est 0.0 (donc désactivé). La valeur de l'option -K limitera le nombre");
        Put_Indent
           ("d'itérations effectuées. Ceci garantira la terminaison du programme. La distance");
        Put_Indent
           ("entre deux vecteurs est définie par la norme euclidienne");
    end Afficher_E_Doc;

    procedure Afficher_P_Doc is
    begin
        Put_Line ("-P");
        Put_Indent
           ("Choisir l'implémentation avec des matrices pleines (la matrice G est calculée).");
        Put_Indent
           ("Cette option est plus rapide mais nécessite plus de mémoire.");
    end Afficher_P_Doc;

    procedure Afficher_C_Doc is
    begin
        Put_Line ("-C");
        Put_Indent
           ("Choisir l'algorithme avec des matrices creuses (la matrice G n'est pas");
        Put_Indent
           ("calculée). C'est l'algorithme qui est mis en œuvre par défaut. Cette option");
        Put_Indent
           ("nécessite moins de mémoire (allocation dynamique) mais est plus lente.");
    end Afficher_C_Doc;

    procedure Afficher_R_Doc is
    begin
        Put_Line ("-R <prefixe>     : String");
        Put_Indent
           ("Choisir le préfixe des fichiers résultats, par défaut ""output"". Les fichiers");
        Put_Indent
           ("produits seront donc <prefixe>.pr pour le classement des nœuds et <prefixe>.prw");
        Put_Indent ("pour les poids.");
    end Afficher_R_Doc;

    procedure Afficher_H_Doc is
    begin
        Put_Line ("-H");
        Put_Indent ("Afficher la documentation complète.");
    end Afficher_H_Doc;

    procedure Afficher_Options_Doc is
    begin
        New_Line;
        Put_Indent ("----------| OPTIONS |----------");
        New_Line;
        Afficher_A_Doc;
        New_Line;
        Afficher_K_Doc;
        New_Line;
        Afficher_E_Doc;
        New_Line;
        Afficher_P_Doc;
        New_Line;
        Afficher_C_Doc;
        New_Line;
        Afficher_R_Doc;
        New_Line;
        Afficher_H_Doc;
    end Afficher_Options_Doc;

    procedure Afficher_Exemples is
    begin
        New_Line;
        Put_Indent ("----------| EXEMPLES D'UTILISATION |----------");
        New_Line;
        Put_Line ("Exemple 1");
        Put_Indent ("> ./pagerank sujet.net");
        New_Line;
        Put_Line
           ("Cet appel lit le fichier sujet.net en version matrice creuse, avec calcul de");
        Put_Line
           ("l'indice 150 et une valeur de alpha à 0.85, epsilon valant 0.0. Il enregistrera");
        Put_Line ("les résultats dans output.pr et output.prw.");

        New_Line;
        Put_Line ("Exemple 2");
        Put_Indent ("> ./pagerank -P -A 0.90 -K 20 sujet2.net");
        New_Line;
        Put_Line
           ("Cet appel lit  le fichier exemple2.net avec une valeur de alpha à 0.90, avec");
        Put_Line
           ("matrice pleine, calcul de l'indice 20 avec epsilon valant 0.0. Il enregistrera");
        Put_Line ("les résultats dans output.pr et output.prw.");

        New_Line;
        Put_Line ("Exemple 3");
        Put_Indent
           ("> ./pagerank -E 0.001 -K 20 -P -E 0.0001 -A 0.70 -C exemple3.txt");
        New_Line;
        Put_Line
           ("Cet appel lit le fichier exemple3.txt avec une valeur de alpha à 0, 70 avec");
        Put_Line
           ("des matrices creuses, et une précision de 0.0001, en s'arrêtant au pire sur le");
        Put_Line
           ("vecteur poids Pi_20. On constate que c'est la dernière occurrence d'une option");
        Put_Line
           ("qui l'emporte sur les précédentes. Il enregistrera les résultats dans output.pr");
        Put_Line ("et output.prw.");

        New_Line;
        Put_Line ("Exemple 4");
        Put_Indent ("> ./pagerank -E 0.01 -K 30 -C -A 1 exemple4.txt");
        New_Line;
        Put_Line
           ("Cet appel lit le fichier exemple3.txt avec une valeur de alpha à 1 en");
        Put_Line
           ("utilisant la version creuse, un epsilon de 0.01 et un calcul de Pi_30 au pire");
        Put_Line
           ("(si la précision souhaitée n'est pas atteinte avant). Il enregistrera les");
        Put_Line ("résultats dans output.pr et output.prw.");

        New_Line;
        Put_Line ("Exemple 5");
        Put_Indent
           ("> ./pagerank -R exemple5 -K 10 -R exemple5-K10 exemple5.txt");
        New_Line;
        Put_Line
           ("Dans les exemples précédents, le préfixe des fichiers résultats n'était pas");
        Put_Line
           ("précisé et le préfixe ""output"" était utilisé par défaut. Ici, les fichiers");
        Put_Line
           ("produits seront donc exemple5-K10.pr exemple5-K10.prw (seule la dernière valeur");
        Put_Line ("de -R est prise en compte).");

        New_Line;
        Put_Line ("Exemple 6");
        Put_Indent ("> ./pagerank -K 500 -K -10 -A 2.5 -C exemple6.txt");
        New_Line;
        Put_Line
           ("Le programme signalera une erreur sur la valeur qui suit l'option -K car elle");
        Put_Line
           ("doit être positive. On pourra s'arrêter à la première erreur ou signaler les");
        Put_Line
           ("erreurs suivantes. Ici la valeur qui suit -A doit être comprise entre 0 et 1.");
    end Afficher_Exemples;

    procedure Afficher_Exemple_Graphe is
    begin
        Put_Indent ("----------| EXEMPLE DE Graphe |----------");
        New_Line;
        Put_Line ("6");
        Put_Line ("0 1");
        Put_Line ("0 2");
        Put_Line ("2 0");
        Put_Line ("2 1");
        Put_Line ("2 4");
        Put_Line ("3 4");
        Put_Line ("3 5");
        Put_Line ("4 3");
        Put_Line ("4 5");
        Put_Line ("5 3");
        New_Line;
        Put_Line ("Explication :");
        Put_Indent
           ("La première ligne indique le nombre de nœuds du graphe (6). Les lignes");
        Put_Indent
           ("suivantes indiquent les arcs du graphe. Ici, le nœud 0 a deux arcs sortants, un");
        Put_Indent
           ("vers le nœud 1 et un vers le nœud 2. Le nœud 2 a trois arcs sortants, un vers le");
        Put_Indent
           ("nœud 0, un vers le nœud 1 et un vers le nœud 4. Le nœud 3 a deux arcs sortants,");
        Put_Indent
           ("un vers le nœud 4 et un vers le nœud 5. Le nœud 4 a deux arcs sortants, un vers");
        Put_Indent
           ("le nœud 3 et un vers le nœud 5. Le nœud 5 a un arc sortant vers le nœud 3.");
        New_Line;
        Put_Indent
           ("Dans le cas où un noeud est répété, on ne le compte qu'une seule fois.");
    end Afficher_Exemple_Graphe;

    procedure Afficher_Doc is
    begin
        New_Line;
        Put_Indent ("-------------------------------------------------");
        Put_Indent ("| Application PageRank - Documentation complète |");
        Put_Indent ("-------------------------------------------------");
        Afficher_Generalites;
        New_Line;
        Afficher_Options_Doc;
        New_Line;
        Afficher_Exemples;
        New_Line;
        Afficher_Exemple_Graphe;
    end Afficher_Doc;

end PageRank_Doc;
