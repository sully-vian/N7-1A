%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP1 - Orthogonalisation de Gram-Schmidt
% mgs.m
%--------------------------------------------------------------------------

function Q = mgs(A)

    % Recuperation du nombre de colonnes de A
    [~, m] = size(A);

    % Initialisation de la matrice Q avec la matrice A
    Q = A;

    for k = 1:m
        % Copier la colonne courante de A dans un vecteur y
        y = A(:, k);

        for i = 1:(k - 1)
            courante = Q(:, i);
            % Calculer la composante de y suivant la colonne courante de Q
            composante = (y' * courante);
            % Retrancher à y sa composante suivant la direction de la colonne courante de Q
            y = y - composante * courante;
        end

        % Normaliser y, le résultat constitue la nouvelle colonne de Q
        Q(:, k) = y / norm(y);
    end

end
