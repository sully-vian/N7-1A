% Fonction parametres_correlation (exercice_1.m)

function [r, a, b] = parametres_correlation(Vd, Vg)

    N = length(Vd);

    moy_d = mean(Vd);
    moy_g = mean(Vg);

    sigma_d = sqrt(1 / N * (Vd' * Vd) - moy_d ^ 2);
    sigma_g = sqrt(1 / N * (Vg' * Vg) - moy_d ^ 2);

    sigma_gd = 1 / N * Vd' * Vg - moy_d * moy_g;

    % Calcul des données finales (voir sujet pour a et b)
    r = sigma_gd / (sigma_d * sigma_g);
    a = sigma_gd / sigma_d ^ 2;
    b =- sigma_gd / sigma_d ^ 2 * moy_d + moy_g;

end
