% Fonction estim_param_Dyx_MC (exercice_1.m)

function [a_Dyx,b_Dyx] = estim_param_Dyx_MC(x_donnees_bruitees,y_donnees_bruitees)

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
    
end