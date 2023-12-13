% fonction estim_param_MC (pour exercice_1.m)

function parametres = estim_param_MC(d, x, y)

    n = length(x);

    % Calcul de A
    A = zeros(n, d);

    for k = 1:d
        A(:, k) = vecteur_bernstein(x, d, k);
    end

    % Calcul de B
    B = y - y(1) * vecteur_bernstein(x, d, 0);

    % Calcul de la solution
    parametres = (A' * A) \ A' * B;

end
