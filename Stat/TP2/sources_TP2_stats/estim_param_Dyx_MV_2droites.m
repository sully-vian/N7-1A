% Fonction estim_param_Dyx_MV_2droites (exercice_2.m) 

function [a_Dyx_1,b_Dyx_1,a_Dyx_2,b_Dyx_2] = ... 
         estim_param_Dyx_MV_2droites(x_donnees_bruitees,y_donnees_bruitees,sigma, ...
                                     tirages_G_1,tirages_psi_1,tirages_G_2,tirages_psi_2)

    % extraction des données utiles
    nb_tirages = length(tirages_psi_1);
    nb_pts = length(x_donnees_bruitees);

    % données bruitées : mise à la bonne taille
    X = repmat(x_donnees_bruitees, 1, nb_tirages);
    Y = repmat(y_donnees_bruitees, 1, nb_tirages);

    % Angles : calcul, mise à la bonne taille 1 & 2
    tan_psi_1 = tan(tirages_psi_1);
    Tan_Psi_1 = repmat(tan_psi_1, nb_pts, 1);

    tan_psi_2 = tan(tirages_psi_2);
    Tan_Psi_2 = repmat(tan_psi_2, nb_pts, 1);

    % G : extraction, mise à la bonne taille 1 & 2
    x_G_1 = tirages_G_1(1, :);
    y_G_1 = tirages_G_1(2, :);
    X_G_1 = repmat(x_G_1, nb_pts, 1);
    Y_G_1 = repmat(y_G_1, nb_pts, 1);

    x_G_2 = tirages_G_2(1, :);
    y_G_2 = tirages_G_2(2, :);
    X_G_2 = repmat(x_G_2, nb_pts, 1);
    Y_G_2 = repmat(y_G_2, nb_pts, 1);

    % calcul des termes à maximiser
    r1 = (Y - Y_G_1) - Tan_Psi_1 .* (X - X_G_1);
    M1 = -r1.^2 / (2*sigma^2);

    r2 = (Y - Y_G_2) - Tan_Psi_2 .* (X - X_G_2);
    M2 = -r2.^2 / (2*sigma^2);

    M = exp(M1) + exp(M2);
    ln_M = log(M);
    M_sum = sum(ln_M);

    % on prend la meilleure colonne
    [~, ind_max] = max(M_sum);
    % On fait le max sur une seule dimension, pas besoin de faire toutes
    % les combinaisons de psi1, psi2, G1 et G2 possibles avec les tirages.
    % On a assez de valeurs comme ça puisqu'on tire BEAUCOUP de valeurs.

    % val de retour 1
    psi_max_1 = tirages_psi_1(ind_max);
    x_G_max_1 = x_G_1(ind_max);
    y_G_max_1 = y_G_1(ind_max);

    a_Dyx_1 = tan(psi_max_1);
    b_Dyx_1 = y_G_max_1 - a_Dyx_1 * x_G_max_1;

    % val de retour 2
    psi_max_2 = tirages_psi_2(ind_max);
    x_G_max_2 = x_G_2(ind_max);
    y_G_max_2 = y_G_2(ind_max);

    a_Dyx_2 = tan(psi_max_2);
    b_Dyx_2 = y_G_max_2 - a_Dyx_2 * x_G_max_2;

end