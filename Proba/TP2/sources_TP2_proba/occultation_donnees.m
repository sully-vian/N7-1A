% Fonction occultation_donnees (donnees_occultees.m)

function [x_donnees_bruitees_visibles, y_donnees_bruitees_visibles] = occultation_donnees(x_donnees_bruitees, y_donnees_bruitees, theta_donnees_bruitees, thetas)

    % Choix de la condition en fonction de l'ordre des angles
    if thetas(1) <= thetas(2)
        condition = (theta_donnees_bruitees >= thetas(1)) & (theta_donnees_bruitees <= thetas(2));
    else
        condition = (theta_donnees_bruitees <= thetas(2)) | (theta_donnees_bruitees >= thetas(1));
    end

    % On ne garde que les points visibles
    x_donnees_bruitees_visibles = x_donnees_bruitees(condition);
    y_donnees_bruitees_visibles = y_donnees_bruitees(condition);

end
