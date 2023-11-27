% fonction vecteur_bernstein (pour exercice_1.m)

function resultat = vecteur_bernstein(x,d,k)

    % Calcul direct 
    resultat = nchoosek(d,k) * (x.^k) .* ((1-x).^(d-k));

end