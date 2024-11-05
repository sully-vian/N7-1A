% Fonction encodage_image (exercice_2.m)

function [I_encodee, dictionnaire, hauteur_I, largeur_I] = encodage_image(I)

    % Obtention des données utiles
    [vecteur_Imin_a_Imax, vecteurs_frequences] = histogramme_normalise(I);
    
    % Magoe (jsp ce qu'est cette fonction)
    dictionnaire = huffmandict(vecteur_Imin_a_Imax, vecteurs_frequences);
    I_encodee = huffmanenco(I(:),dictionnaire);

    [hauteur_I, largeur_I] = size(I);

end
