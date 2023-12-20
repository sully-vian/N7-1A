% fonction calcul_bon_partitionnement (pour l'exercice 1)

function meilleur_pourcentage_partitionnement = calcul_bon_partitionnement(Y_pred,Y)
    
    k = 3; % nb de classes
    N = length(Y); % nb de fleurs

    % initialisation
    meilleur_pourcentage_partitionnement = 0;

    % on teste toutes les permutations
    permutations = perms(1:k);
    for i=1:length(permutations)

        p = permutations(i,:);

        % calcul de Y permuté selon p
        Y_perm = Y_pred;
        Y_perm(Y_pred == 1) = p(1);
        Y_perm(Y_pred == 2) = p(2);
        Y_perm(Y_pred == 3) = p(3);

        % calcul pourcentage courant
        Y_exact = Y_perm - Y;
        pourcentage = length(Y_exact(Y_exact == 0)) / N * 100;

        % m-a-j meilleur pourcentage
        if pourcentage > meilleur_pourcentage_partitionnement
            meilleur_pourcentage_partitionnement = pourcentage;
        end
    end
end