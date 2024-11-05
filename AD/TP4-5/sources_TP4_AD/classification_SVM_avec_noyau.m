% fonction classification_SVM_avec_noyau (pour l'exercice 2)

function Y_pred = classification_SVM_avec_noyau(X, sigma, X_VS, Y_VS, Alpha_VS, c)

    N = length(X);
    N_VS = length(X_VS);

    % Calcul de G matrice de Gram tq K(i,j) = K(xi,xj)
    G = zeros(N, N_VS);

    for i = 1:N
        xi = X(i, :);

        for j = 1:N_VS
            xj = X_VS(j, :);
            G(i, j) = exp(- norm(xi - xj) ^ 2 / (2 * sigma ^ 2));
        end

    end

    Y_pred = zeros(N, 1);

    for i = 1:N
        Y_pred(i) = sign(Alpha_VS' * (Y_VS .* G(i, :)') - c);
    end

end
