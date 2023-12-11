% fonction modelisation_vraisemblance (pour l'exercice 1)

function modele_V = modelisation_vraisemblance(X, mu, Sigma)

    n = length(X);
    modele_V = zeros(n, 1);
    mu = mu'; % on met mu vertical

    for i = 1:n
        x = X(i, :)'; % on met x vertical
        px = 1 / (2 * pi * sqrt(det(Sigma))) * exp(-1/2 * (x - mu)' * inv(Sigma) * (x - mu));
        modele_V(i, :) = px;
    end

end
