% Fonction decorrelation_colonnes (exercice_2.m)

function I_decorrelee = decorrelation_colonnes(I)

    % Initiaisation de l'image décorrélée
    I_decorrelee = I;

    % calcul des images réduites
    Id = I(:, 2:end);
    Ig = I(:, 1:end - 1);

    % Calcul de l'image décorrélée
    I_decorrelee(:, 2:end) = Id - Ig;

end
