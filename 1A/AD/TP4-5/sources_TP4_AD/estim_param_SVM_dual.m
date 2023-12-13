% fonction estim_param_SVM_dual (pour l'exercice 1)

function [X_VS, w, c, code_retour] = estim_param_SVM_dual(X, Y)

    % Y est un vecteur colonne
    % Y_diag est carré avec les Y(i) sur la diag
    size(Y)
    N = length(X);
    eps = 10e-6;

    Y_diag = diag(Y);

    % à minimiser
    H = Y_diag * (X * X') * Y_diag;
    f = -ones(N, 1);

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

    X_VS = X(alpha > eps, :);
    Y_VS = Y(alpha > eps);
    w = zeros(2, 1);

    % Calcul de w pitit à piti
    for i = 1:N
        x = X(i, :)';
        w = w + alpha(i) * Y(i) .* x;

    end

    c = w' * X_VS(1, :)' - Y_VS(1);

end
