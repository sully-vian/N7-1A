% Fonction estim_param_Dyx_MC1 (exercice_2.m)

function [a_Dyx, b_Dyx, coeff_R2] = estim_param_Dyx_MC1(x_donnees_bruitees, y_donnees_bruitees)

    n = length(x_donnees_bruitees);

    y_G = mean(y_donnees_bruitees);
    y_c = y_donnees_bruitees - y_G;

    % param pour résolution moindres carrés
    A = [x_donnees_bruitees ones(n, 1)];
    b = y_donnees_bruitees;

    % résolution moindes carrés
    X = A \ b;
    a_Dyx = X(1);
    b_Dyx = X(2);

    SCT = y_c' * y_c; % Somme des Carrés Totale
    estim = y_donnees_bruitees - a_Dyx * x_donnees_bruitees - b_Dyx;
    SCR = estim' * estim; % Somme des Carrés Résiduelle

    coeff_R2 = 1 - SCR / SCT;

end
