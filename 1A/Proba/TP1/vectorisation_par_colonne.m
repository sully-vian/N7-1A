% Fonction vectorisation_par_colonne (exercice_1.m)

function [Vg, Vd] = vectorisation_par_colonne(I)

    Id = I(:, 2:end);
    Vd = Id(:);
    Ig = I(:, 1:end - 1);
    Vg = Ig(:);

end
