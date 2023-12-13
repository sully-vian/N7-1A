% fonction qualite_classification (pour l'exercice 2)

function [pourcentage_bonnes_classifications_total, pourcentage_bonnes_classifications_fibrome, ...
              pourcentage_bonnes_classifications_melanome] = qualite_classification(Y_pred, Y)

n = length(Y);

% on remplit les cases
classes = [0 0 0 0];

% Nb de pts de chaque classe
n1 = nnz(Y(:) == 1);
n2 = n - n1;

% Compte (illisible) des bons
for i = 1:n
    y = Y(i);
    y_pred = Y_pred(i);
    classes(y * y_pred) = classes(y * y_pred) + 1;
end

% Calcul des données finales
pourcentage_bonnes_classifications_fibrome = classes(1) * 100 / n1;
pourcentage_bonnes_classifications_melanome = classes(4) * 100 / n2;
pourcentage_bonnes_classifications_total = (classes(1) + classes(4)) * 100 / n;

end
