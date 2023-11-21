function [beta, norm_grad_f_beta, f_beta, norm_delta, nb_it, exitflag] ...
          = Algo_Newton(Hess_f_C14, beta0, option)
%************************************************************
% Fichier  ~gergaud/ENS/Optim1a/TP-optim-20-21/Newton_ref.m *
% Novembre 2020                                             *
% Universit√© de Toulouse, INP-ENSEEIHT                      *
%************************************************************
%
% Newton rÈsout par l'algorithme de Newton les problemes aux moindres carres
% Min 0.5||r(beta)||^2
% beta \in R^p
%
% Parametres en entrees
% --------------------
% Hess_f_C14 : fonction qui code la hessiennne de f
%              Hess_f_C14 : R^p --> matrice (p,p)
%              (la fonction retourne aussi le residu et la jacobienne)
% beta0  : point de d√©part
%          real(p)
% option(1) : Tol_abs, tol√©rance absolue
%             real
% option(2) : Tol_rel, tol√©rance relative
%             real
% option(3) : n_itimax, nombre d'it√©rations maximum
%             integer
%
% Parametres en sortie
% --------------------
% beta      : beta
%             real(p)
% norm_gradf_beta : ||gradient f(beta)||
%                   real
% f_beta    : f(beta)
%             real
% res       : r(beta)
%             real(n)
% norm_delta : ||delta||
%              real
% nb_it       : nombre d'it√©rations
%              integer
% exitflag   : indicateur de sortie
%              integer entre 1 et 4
% exitflag = 1 : ||gradient f(beta)|| < max(Tol_rel||gradient f(beta0)||,Tol_abs)
% exitflag = 2 : |f(beta^{k+1})-f(beta^k)| < max(Tol_rel|f(beta^k)|,Tol_abs)
% exitflag = 3 : ||delta|| < max(Tol_rel delta^k),Tol_abs)
% exitflag = 4 : nombre maximum d'it√©rations atteint
%      
% ---------------------------------------------------------------------------------

% TO DO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    nb_it = 0;
    exitflag = 0;

    % extraction des options
    Tol_abs = option(1);
    Tol_rel = option(2);
    n_itmax = option(3);

    beta1 = beta0;

    while exitflag == 0
        % calcul des valeurs utiles
        H_f = Hess_f_C14(beta1);
        grad_f = J_res * res;

        % relation de rÈcurrence sur beta
        beta2 = beta1 - H_f \ grad_f;

        % m-a-j des donnÈes
        norm_grad_f_beta = norm(grad_f);
        f_beta1 = 1/2 * norm(residu(beta1))^2;
        f_beta2 = 1/2 * norm(residu(beta2))^2;
        delta = beta2 - beta1;
        

        % m-a-j de conditions d'arret
        if norm_grad_f_beta < max(Tol_rel * norm_grad_f_beta0, Tol_abs)
                exitflag = 1;
        elseif abs(f_beta2 - f_beta1) < max(Tol_rel * abs(f_beta1), Tol_abs)
                exitflag = 2;
        elseif norm(delta) < max(Tol_rel * norm(beta1),Tol_abs)
                exitflag = 3;
        elseif nb_it == n_itmax
                exitflag = 4;
        end

        beta1 = beta2;
    end

    % calcul final des donnÈes manquantes
    beta = beta2;
    norm_grad_f_beta = norm(grad_f);
    f_beta = 1/2 * norm(residu(beta))^2;
    norm_delta = norm(delta);


end