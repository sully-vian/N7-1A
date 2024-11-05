% Fonction estim_param_F (exercice_1.m)

function [rho_F, theta_F, ecart_moyen] = estim_param_F(rho, theta)

    A = [cos(theta) sin(theta)];
    B = rho;

    % Résolution du système
    X = A \ B;

    x_F = X(1);
    y_F = X(2);

    rho_F = sqrt(x_F ^ 2 + y_F ^ 2);
    theta_F = atan2(y_F, x_F);

    ecarts = abs(rho - rho_F * cos(theta - theta_F));
    ecart_moyen = mean(ecarts);

end
