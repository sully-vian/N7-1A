%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP1 - Orthogonalisation de Gram-Schmidt
% cgs.m
%--------------------------------------------------------------------------

function Q = cgs(A)

    % Recuperation du nombre de colonnes de A
    [n, m] = size(A);

    % Initialisation de la matrice Q avec la matrice A
    Q = A;

    for k = 1:m
        y = A(:, k);
        tmp = zeros(n, 1);

        for i = 1:(k - 1)
            courante = Q(:, i);
            tmp = tmp + (y' * courante) * courante;
        end

        y = y - tmp;
        Q(:, k) = y / norm(y);
    end

end
