% Fonction estimation_C_et_R (exercice_3.m)

function [C_estime, R_estime, ecart_moyen] = ...
        estimation_C_et_R(x_donnees_bruitees, y_donnees_bruitees, C_tests, R_tests)

    M = length(C_tests);
    ecart_tot = 0;

    % Initialisation du min
    C_estime = C_tests(1, :);
    R_estime = R_tests(1);
    distances = sqrt((x_donnees_bruitees - C_estime(1)) .^ 2 + (y_donnees_bruitees - C_estime(2)) .^ 2);
    S_min = (distances - R_estime)' * (distances - R_estime);

    % test pour tous les tirages
    for i = 2:M
        C = C_tests(i, :);
        R = tirages_R(i);
        distances = sqrt((x_donnees_bruitees - C(1)) .^ 2 + (y_donnees_bruitees - C(2)) .^ 2);
        ecart_tot = ecart_tot + distance;
        S = (distances - R)' * (distances - R);

        if S < S_min
            C_estime = C;
            R_estime = R;
            S_min = S;
        end

    end

    ecart_moyen = ecart_tot / M;

end
