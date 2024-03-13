%--------------------------------------------------------------------------
% ENSEEIHT - 1SN - Calcul scientifique
% TP2 - Factorisation LU
% tp.m
%--------------------------------------------------------------------------

clear
close all
clc

% Choix de la matrice
choice_matrix=menu('Choose a matrix','bodyy4','nos4','bcsstk09','bcsstk05','bcsstk27','nos1','build');
switch choice_matrix
    case 1
        load Matrices/bodyy4   % n = 100
    case 2
        load Matrices/nos4     % n = 17546
    case 3
        load Matrices/bcsstk09 % n = 1083
    case 4
        load Matrices/bcsstk05 % n = 153
    case 5
        load Matrices/bcsstk27 % n = 1224
    case 6
        load Matrices/nos1     % n = 237
end

if (choice_matrix < 7)
    A = full(Problem.A);
    clear Problem;
else 
  n = 500;
  U = gallery('orthog',n);
  tmp = randn(n,1);
  D = diag(tmp);  
  A = U*D*U';
end

[n,~] = size(A);
norm_A = norm(A,'fro');

% Solution exacte
x_exact = ones(n,1);

%Second membre 
b = A * x_exact;

%Information sur les permutations des lignes
p = 1:n; % init à permutation identité

% Factorisation de LU de A
%%% TODO %%%
disp('Factorisation LU')

for k = 1:n-1
    if (abs(A(k,k)) < eps)
        % exit
    else
        A(k+1:n, k) = A(k+1:n, k) / A(k,k);
        A(k+1:n, k+1:n) = A(k+1:n, k+1:n) - A(k+1:n, k) * A(k, k+1:n);
    end
end

%{
for k = 1:n-1
    [~, i] = max(abs(A(k:n, k))); % pivot search
    i = i + k-1; % on le replace dans le ref de la matrice A entière
    if (abs(A(i,k)) <= eps*norm_A)
        % exit since A is numerically singular
    else
        A([i k], :) = A([k i], :); % swap rows
        b([i k]) = b([k i]); % swap entrees
        p([i k]) = p([k i]);

        A(k+1:n, k) = A(k+1:n, k) / A(k,k);
        A(k+1:n, k+1:n) = A(k+1:n, k+1:n) - A(k+1:n, k) * A(k, k+1:n);
    end
end
%}

%%% FIN TO DO %%%
%Resolution du systeme triangulaire inferieur
y = descente(A,p,b);

%Resolution du systeme triangulaire superieur
x = remontee(A,y);

%Calcul des erreurs directe et inverse
[err_d,err_i] = erreur(A,b,x,x_exact)
