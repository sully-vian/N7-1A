% Fonction tirages_aleatoires (exercice_1.m)

function tirages_angles = tirages_aleatoires_uniformes(n_tirages)

    % Tirages aleatoires d'angles : moyenne = 0 / demi-repartition = pi/2
    tirages_angles = rand(n_tirages, 1) * pi - pi / 2;

end
