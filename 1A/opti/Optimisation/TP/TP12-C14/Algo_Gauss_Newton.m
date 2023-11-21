function [beta, norm_grad_f_beta, f_beta, norm_delta, nb_it, exitflag] ...
          = Algo_Gauss_Newton(residu, J_residu, beta0, option)
%*****************************************************************
% Fichier  ~gergaud/ENS/Optim1a/TP-optim-20-21/TP-ref/GN_ref.m   *
% Novembre 2020                                                  *
% Université de Toulouse, INP-ENSEEIHT                           *
%*****************************************************************
%
% GN resout par l'algorithme de Gauss-Newton les problemes aux moindres carres
% Min 0.5||r(beta)||^2
% beta \in \IR^p
%
% Paramètres en entrés
% --------------------
% residu : fonction qui code les résidus
%          r : \IR^p --> \IR^n
% J_residu : fonction qui code la matrice jacobienne
%            Jr : \IR^p --> real(n,p)
% beta0 : point de départ
%         real(p)
% option(1) : Tol_abs, tolérance absolue
%             real
% option(2) : Tol_rel, tolérance relative
%             real
% option(3) : n_itmax, nombre d'itérations maximum
%             integer
%
% Paramètres en sortie
% --------------------
% beta      : beta
%             real(p)
% norm_gradf_beta : ||gradient f(beta)||
%                   real
% f_beta : f(beta)
%          real
% r_beta : r(beta)
%          real(n)
% norm_delta : ||delta||
%              real
% nb_it : nombre d'itérations
%        integer
% exitflag   : indicateur de sortie
%              integer entre 1 et 4
% exitflag = 1 : ||gradient f(beta)|| < max(Tol_rel||gradient f(beta0)||,Tol_abs)
% exitflag = 2 : |f(beta^{k+1})-f(beta^k)| < max(Tol_rel|f(beta^k)|,Tol_abs)
% exitflag = 3 : ||delta|| < max(Tol_rel delta^k),Tol_abs)
% exitflag = 4 : nombre maximum d'itérations atteint
%      
% ---------------------------------------------------------------------------------
    
    nb_it = 0;
    exitflag = 0;
    res0 = residu(beta0);
    J_res0 = J_residu(beta0);
    grad_f0 = J_res0' * res0;
    norm_grad_f_beta0 = norm(grad_f0);

    % extraction des options
    Tol_abs = option(1);
    Tol_rel = option(2);
    n_itmax = option(3);

    beta1 = beta0;

    % itérations (convergence vers solution)
    while exitflag == 0
        % calcul des valeurs utiles
        res1 = residu(beta1);
        J_res1 = J_residu(beta1);
        grad_f = J_res1' * res1;

        % relation de récurrence, on avance ça rime !)
        beta2 = beta1 - (J_res1' * J_res1) \ grad_f; % inv(A)B pas rapide -> A\B ou mldivide(A,B)
        
        % m-a-j des données
        norm_grad_f_beta = norm(grad_f);
        f_beta1 = 1/2 * norm(res1)^2;
        f_beta2 = 1/2 * norm(residu(beta2))^2;
        delta = beta2 - beta1;
        
        nb_it = nb_it + 1;

        % m-a-j des conditions d'arret
        if norm_grad_f_beta < max(Tol_rel * norm_grad_f_beta0, Tol_abs)
            exitflag = 1;

        elseif abs(f_beta2 - f_beta1) < max(Tol_rel * abs(f_beta1),Tol_abs)
            exitflag = 2;
        

        elseif norm(delta) < max(Tol_rel * norm(beta1), Tol_abs)
            exitflag = 3;

        elseif nb_it == n_itmax
            exitflag = 4;
        end

        beta1 = beta2;
    end

    % calcul final des données manquantes
    beta = beta2;
    norm_grad_f_beta = norm(grad_f);
    f_beta = 1/2 * norm(residu(beta))^2;
    norm_delta = norm(delta);

end