% function correlation_contraste (pour exercice_1.m)

function [correlation, contraste] = correlation_contraste(X)

    n = length(X);

    % calcul des individus centrés et de la matrice de covariance
    Xc = X - mean(X);
    Sigma = 1 / n * (Xc' * Xc);

    % extraction des données de Sigma
    SigmaR2 = Sigma(1, 1);
    SigmaV2 = Sigma(2, 2);
    SigmaB2 = Sigma(3, 3);
    SigmaRV = Sigma(1, 2);
    SigmaRB = Sigma(1, 3);
    SigmaVB = Sigma(2, 3);
    SigmaR = sqrt(SigmaR2);
    SigmaV = sqrt(SigmaV2);
    SigmaB = sqrt(SigmaB2);

    %calcul des corrélations
    rRV = SigmaRV / (SigmaR * SigmaV);
    rRB = SigmaRB / (SigmaR * SigmaB);
    rVB = SigmaVB / (SigmaV * SigmaB);
    correlation = [rRV, rRB, rVB];

    % calcul des contrastes
    cR = SigmaR2 / (SigmaR2 + SigmaV2 + SigmaB2);
    cV = SigmaV2 / (SigmaR2 + SigmaV2 + SigmaB2);
    cB = SigmaB2 / (SigmaR2 + SigmaV2 + SigmaB2);
    contraste = [cR, cV, cB];
end
