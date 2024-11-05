% Fonction tirages_aleatoires_uniformes (exercice_1.m)

function [tirages_angles,tirages_G] = tirages_aleatoires_uniformes(n_tirages,taille)

    % Tirages aleatoires d'angles : moyenne = 0 / demi-repartition = pi/2
    tirages_angles = rand(1, n_tirages) * pi - pi/2;
    % Tirages aleatoires de points pour se trouver sur la droite (sur [-20,20])
    tirages_G = rand(2, n_tirages) * 40 - 20;

end