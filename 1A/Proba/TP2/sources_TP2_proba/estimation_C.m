% Fonction estimation_C (exercice_2.m)

function C_estime = estimation_C(x_donnees_bruitees, y_donnees_bruitees, tirages_C, R_moyen)

    M = length(tirages_C);

    % Initialisation du min
    C_estime = tirages_C(1, :);
    distances = sqrt((x_donnees_bruitees - C_estime(1)) .^ 2 + (y_donnees_bruitees - C_estime(2)) .^ 2);
    S_min = (distances - R_moyen)' * (distances - R_moyen);

    % test pour tous les C générés
    for i = 2:M
        C = tirages_C(i, :);
        distances = sqrt((x_donnees_bruitees - C(1)) .^ 2 + (y_donnees_bruitees - C(2)) .^ 2);
        S = (distances - R_moyen)' * (distances - R_moyen);

        if S < S_min
            C_estime = C;
            S_min = S;
        end

    end

end
