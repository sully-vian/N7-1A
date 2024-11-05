% function ACP (pour exercice_2.m)

function [C, bornes_C, coefficients_RVG2gris] = ACP(X)

    n = length(X);

    % calcul des individus centrés et de la matrice de covariance
    Xc = X - mean(X);
    Sigma = 1 / n * (Xc' * Xc);

    % calcul des valeurs propes et vecteurs propres, puis tri décroissant
    [W, D] = eig(Sigma);
    [~, indice] = sort(diag(D), 'descend');
    W = W(:, indice); % matrice de passage de ACP vers canonique

    % calcul des coordonnées dans la base de vect propres
    % inv(W) pr passer dans l'autre sens
    C = (W \ Xc'); % Xc' car les lignes sont les vecteurs à passer
    C = C'; % On remet C dans le bon format
    %C = Xc*W;

    % calcul bornes pr avoir graphe à l'échelle
    bornes_C = [min(C, [], "all") max(C, [], "all")];

    %
    coefficients_RVG2gris = W(:, 1) / norm(W(:, 1), 1);

end
