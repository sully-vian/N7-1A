% Fonction estim_param_Dyx_MV (exercice_1.m)

function [a_Dyx, b_Dyx, residus_Dyx] = estim_param_Dyx_MV(x_donnees_bruitees, y_donnees_bruitees, tirages_psi)

    % extraction des données utiles
    [x_G, y_G, x_c, y_c] = centrage_des_donnees(x_donnees_bruitees, y_donnees_bruitees);
    nb_tirages = length(tirages_psi);

    nb_pts = length(x_c);
    tan_psi = tan(tirages_psi);

    % mise à la taille
    X_C = repmat(x_c, 1, nb_tirages);
    Y_C = repmat(y_c, 1, nb_tirages);

    Tan_Psi = repmat(tan_psi, nb_pts, 1);

    % calcul des termes à minimiser
    M = Y_C - X_C .* Tan_Psi;
    M2 = M .^ 2;
    M_sum = sum(M2);

    % on prend la meilleure colonne
    [~, ind_min] = min(M_sum);
    psi_min = tirages_psi(ind_min);

    % val de retour
    a_Dyx = tan(psi_min);
    b_Dyx = y_G - a_Dyx * x_G;
    residus_Dyx = M(:, ind_min);

end
