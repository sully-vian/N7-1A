% Fonction estim_param_Dyx_MC1 (exercice_2.m)

function [a_Dyx,b_Dyx,coeff_R2] = ...
                   estim_param_Dyx_MC1(x_donnees_bruitees,y_donnees_bruitees)

    n = length(x_donnees_bruitees);

    x_G = mean(x_donnees_bruitees);
    y_G = mean(y_donnees_bruitees);
    x_c = x_donnees_bruitees - x_G;
    y_c = y_donnees_bruitees - y_G;
   

    A = [x_donnees_bruitees ones(n,1)];

    b = y_donnees_bruitees;

    X = A\b;
    a_Dyx = X(1);
    b_Dyx = X(2);

    SCR = 0;
    SCT = y_c' * y_c;

    coeff_R2 = 1 - SCR/SCT;

    
end