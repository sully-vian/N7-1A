function L = laplacian(nu, dx1, dx2, N1, N2)
    %
    %  Cette fonction construit la matrice de l'op�rateur Laplacien 2D anisotrope
    %
    %  Inputs
    %  ------
    %
    %  nu : nu=[nu1;nu2], coefficients de diffusivit� dans les dierctions x1 et x2.
    %
    %  dx1 : pas d'espace dans la direction x1.
    %
    %  dx2 : pas d'espace dans la direction x2.
    %
    %  N1 : nombre de points de grille dans la direction x1.
    %
    %  N2 : nombre de points de grilles dans la direction x2.
    %
    %  Outputs:
    %  -------
    %
    %  L      : Matrice de l'op�rateur Laplacien (dimension N1N2 x N1N2)
    %
    %

    % Données ui
    nu1 = nu(1);
    nu2 = nu(2);
    N = N1 * N2;

    % Cr�ation des vecteurs diagonaux
    vh = zeros(N, 1) - 2 * (nu1 / dx1 ^ 2 + nu2 / dx2 ^ 2);
    vh1 = ones(N, 1) * nu1 / dx1 ^ 2;
    vh2haut = ones(N, 1) * nu2 / dx2 ^ 2;
    vh2bas = ones(N, 1) * nu2 / dx2 ^ 2;

    % Rajout des 0 (effet de bord)
    vh2haut(1:N2:end) = 0;
    vh2bas(N2:N2:end) = 0;

    % Matrices carr�es N*N avec les diags bien plac�es
    Lh = spdiags(vh, 0, N, N);
    Lh1 = spdiags([vh1 vh1], [-N2 N2], N, N);
    Lh2 = spdiags([vh2bas vh2haut], [-1 1], N, N);

    % Calcul final de L
    L =- (Lh + Lh1 + Lh2);

end
