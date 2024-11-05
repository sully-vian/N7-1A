% Fonction calcul_G_et_R_moyen (exercice_3.m)

function [G, R_moyen, distances] = calcul_G_et_R_moyen(x_donnees_bruitees, y_donnees_bruitees)

    % Calcul de G
    x_G = mean(x_donnees_bruitees);
    y_G = mean(y_donnees_bruitees);
    G = [x_G; y_G];

    % Calcul du rayon moyen
    R2 = (x_donnees_bruitees - x_G) .^ 2 + (y_donnees_bruitees - y_G) .^ 2;
    distances = sqrt(R2);
    R_moyen = mean(distances);

end
