% fonction estim_param_SVM_noyau (pour l'exercice 2)

function [X_VS, Y_VS, Alpha_VS, c, code_retour] = estim_param_SVM_noyau(X, Y, sigma)

    % Y est un vecteur colonne
    % Y_diag est carré avec les Y(i) sur la diag
    Y_diag = diag(Y);

    N = length(X);
    eps = 10e-6;

    % Calcul de G matrice de Gram tq K(i,j) = K(xi,xj)
    G = zeros(N);

    for i = 1:N
        xi = X(i, :);

        for j = 1:N
            xj = X(j, :);
            G(i, j) = exp(- norm(xi - xj) ^ 2 / (2 * sigma ^ 2));
        end

    end

    % à minimiser
    H = Y_diag * G * Y_diag;
    f =- ones(N, 1);

    % Pas de contrainte d'inégalité
    A = [];
    b = [];

    % Aeq * alpha = 0
    Aeq = Y';
    beq = 0;

    % 0 <= alpha(i)
    lb = zeros(N, 1);
    ub = []; % alpha(i) pas majoré

    [alpha, ~, code_retour, ~] = quadprog(H, f, A, b, Aeq, beq, lb, ub);
    Alpha_VS = alpha(alpha > eps);

    % récupération des vecteurs de support
    X_VS = X(alpha > eps, :);
    Y_VS = Y(alpha > eps);

    % calcul de c (sans w cette fois)
    c = alpha' * (Y .* G(:, 1)) - Y(1);

end
