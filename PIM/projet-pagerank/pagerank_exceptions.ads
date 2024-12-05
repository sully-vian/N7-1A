-- Définition des exceptions pour le calcul du Pagerank

package PageRank_Exceptions is

    Arg_Option_Exception : exception; -- Erreur sur l'entrée d'argument

    Fichier_Introuvable_Exception : exception; -- le fichier entré n'est pas trouvé

    Fichier_Format_Exception : exception; -- le fichier entré n'est pas dans le bon format

    Fichier_Vide_Exception : exception; -- le fichier entré est vide

    Afficher_Doc_Exception : exception; -- la documentation a été demandée (et affichée)

end PageRank_Exceptions;
