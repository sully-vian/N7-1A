% fonction classification_MV (pour l'exercice 2)

function Y_pred_MV = classification_MV(X, mu_1, Sigma_1, mu_2, Sigma_2)

    n = length(X);
    Y_pred_MV = ones(n, 1);

    X_app1 = modelisation_vraisemblance(X, mu_1, Sigma_1);
    X_app2 = modelisation_vraisemblance(X, mu_2, Sigma_2);

    Y_pred_MV(X_app1 <= X_app2) = 2;

end
