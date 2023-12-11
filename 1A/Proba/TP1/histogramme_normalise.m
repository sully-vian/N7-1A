% Fonction histogramme_normalise (exercice_2.m)

function [vecteurs_frequences, vecteur_Imin_a_Imax] = histogramme_normalise(I)

    Imin = min(I(:));
    Imax = max(I(:));
    vecteur_de_decoupage = [Imin:1:Imax + 1];

    h = histcounts(I, vecteur_de_decoupage);
    vecteurs_frequences = h / sum(h);
    vecteur_Imin_a_Imax = [Imin:Imax];

end
