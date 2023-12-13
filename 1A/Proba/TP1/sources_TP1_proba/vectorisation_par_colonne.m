% Fonction vectorisation_par_colonne (exercice_1.m)
% Vd est le vecteur des voisins de droite
% Vg est le vecteur des voisins de gauche

function [Vd, Vg] = vectorisation_par_colonne(I)

    % Calcul de Vd et VG en matrice
    Vd = I(:, 2:end);
    Vg = I(:, 1:end - 1);

    % Vectorisations
    Vg = Vg(:);
    Vd = Vd(:);

end
