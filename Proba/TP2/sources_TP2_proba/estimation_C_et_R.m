% Fonction estimation_C_et_R (exercice_3.m)

function [C_estime, R_estime] = estimation_C_et_R(x_donnees_bruitees, y_donnees_bruitees, tirages_C, tirages_R)

    M = length(tirages_C);

    % Initialisation du min
    C_estime = tirages_C(1, :);
    R_estime = tirages_R(1);
    distances = sqrt((x_donnees_bruitees - C_estime(1)) .^ 2 + (y_donnees_bruitees - C_estime(2)) .^ 2);
    S_min = (distances - R_estime)' * (distances - R_estime);

    % test pour tous les tirages
    for i = 2:M
        C = tirages_C(i, :);
        R = tirages_R(i);
        distances = sqrt((x_donnees_bruitees - C(1)) .^ 2 + (y_donnees_bruitees - C(2)) .^ 2);
        S = (distances - R)' * (distances - R);

        if S < S_min
            C_estime = C;
            R_estime = R;
            S_min = S;
        end

    end

end
