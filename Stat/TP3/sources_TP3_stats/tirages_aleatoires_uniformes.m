% Fonction tirages_aleatoires (exercice_3.m)

function [tirages_C, tirages_R] = tirages_aleatoires_uniformes(n_tirages, G, R_moyen)

    % Coordonnées tirées de C
    tirages_x = rand(n_tirages, 1) * 2 * R_moyen + G(1) - R_moyen;
    tirages_y = rand(n_tirages, 1) * 2 * R_moyen + G(2) - R_moyen;

    % Calcul de C
    tirages_C = [tirages_x tirages_y];

    % Tirages de R
    tirages_R = rand(n_tirages, 1) * R_moyen + R_moyen / 2;

end
