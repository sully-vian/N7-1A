% Fonction decorrelation_colonnes (exercice_2.m)

function I_decorrelee = decorrelation_colonnes(I)

    I_decorrelee = I;

    I_decorrelee(:, 2:end) = I(:, 2:end) - I(:, 1:end - 1);

end
