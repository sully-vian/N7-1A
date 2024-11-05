% fonction estim_param_MC_paire (pour exercice_2.m)

function parametres = estim_param_MC_paire(d, x, y_inf, y_sup)

    n = length(x);

    % Concaténation pour avoir y tout entier
    y = [y_inf, y_sup];

    % Calcul de A
    A = zeros(2 * n, 2 * d - 1);

    for k = 1:d - 1
        v = vecteur_bernstein(x, d, k);
        A(1:n, k) = v;
        A(n:end, k + (d - 1)) = v;
    end

    %   Récupération des conditions de départ
    yinf1 = repmat(y_inf(1), n);
    ysup1 = repmat(y_sup(1), n);
    y1 = [yinf1, ysup1];

    % Calcul de B
    B = y - y1 .* vecteur_bernstein(x, d, 0);

    % Calcul de la solution
    parametres = (A' * A) \ A' * B;

end
