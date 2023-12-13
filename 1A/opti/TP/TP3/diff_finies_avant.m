% Auteur : J. Gergaud
% décembre 2017
% -----------------------------
%

function Jac = diff_finies_avant(fun, x, option)
    %
    % Cette fonction calcule les différences finies avant sur un schéma
    % Paramètres en entrées
    % fun : fonction dont on cherche à calculer la matrice jacobienne
    %       fonction de IR^n à valeurs dans IR^m
    % x   : point où l'on veut calculer la matrice jacobienne
    % option : précision du calcul de fun (ndigits)
    %
    % Paramètre en sortie
    % Jac : Matrice jacobienne approximé par les différences finies
    %        real(m,n)
    % ------------------------------------

    % Dimensions entrée/sortie
    n = length(x);
    m = length(fun(x));

    % Initialisation Jacobienne
    Jac = zeros(m, n);

    % Calcul de h
    ndigits = option;
    omega = 10 ^ (-ndigits);
    h = sqrt(omega) * max(abs(x), 1) .* sgn(x);

    % Calcul vecteurs utiles
    H = diag(h);
    XPlusH = repmat(x, 1, n) + H;

    % remplissage de Jac
    for i = 1:n
        Jac(:, i) = (fun(XPlusH(:, i)) - fun(x)) / h(i);
    end

end

function s = sgn(x)
    % fonction signe qui renvoie 1 si x = 0
    if x == 0
        s = 1;
    else
        s = sign(x);
    end

end
