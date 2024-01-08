% Fonction RANSAC_2droites (exercice_2.m)

function [rho_F_estime, theta_F_estime] = RANSAC_2droites(rho, theta, parametres)

    % Parametres de l'algorithme RANSAC :
    S_ecart = parametres(1); % seuil pour l'ecart
    S_prop = parametres(2); % seuil pour la proportion
    k_max = parametres(3); % nombre d'iterations
    n_donnees = length(rho);
    ecart_moyen_min = Inf;

    for i = 1:k_max
        % choix deux incices random
        idx = randperm(n_donnees, 2);
        i1 = idx(1);
        i2 = idx(2);

        % set les 2 droites qu'on va utiliser et estimation de leur pt
        % d'intersection
        rho_i = [rho(i1); rho(i2)];
        theta_i = [theta(i1); theta(i2)];
        [rho_F, theta_F, ~] = estim_param_F(rho_i, theta_i);

        ecart = abs(rho - rho_F * cos(theta - theta_F));
        id_conformes = ecart < S_ecart;
        nb_conformes = sum(id_conformes);

        rho_conformes = rho(id_conformes == 1);
        theta_conformes = theta(id_conformes == 1);

        % accepté ?
        if (nb_conformes / n_donnees > S_prop)
            [rho_F, theta_F, ecart_moyen] = estim_param_F(rho_conformes, theta_conformes);

            % nouveau meilleur ?
            if (ecart_moyen < ecart_moyen_min)
                rho_F_estime = rho_F;
                theta_F_estime = theta_F;
            end

        end

    end

end
