% fonction classification_MAP (pour l'exercice 3)

function Y_pred_MAP = classification_MAP(X,p1,mu_1,Sigma_1,mu_2,Sigma_2)

    n = length(X);
    Y_pred_MV = ones(n,1);
    p2 = 1 - p1;

    V1 = modelisation_vraisemblance(X, mu_1, Sigma_1);
    V2 = modelisation_vraisemblance(X, mu_2, Sigma_2);

    X_app1 = p1 * V1;
    X_app2 = p2 * V2;

    Y_pred_MV(X_app1 <= X_app2) = 2;

end
