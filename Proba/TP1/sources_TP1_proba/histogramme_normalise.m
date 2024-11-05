% Fonction histogramme_normalise (exercice_2.m)
% pour creer un hstogramme des valeurs de gris d'une image

function [vecteur_Imin_a_Imax, vecteurs_frequences] = histogramme_normalise(I)

    N = length(I(:));

    % Niveaux de gris min et max
    Imin = min(I(:));
    Imax = max(I(:));

    vecteurs_frequences = histcounts(I(:), Imin:Imax + 1) / N;

    vecteur_Imin_a_Imax = Imin:Imax;

end
