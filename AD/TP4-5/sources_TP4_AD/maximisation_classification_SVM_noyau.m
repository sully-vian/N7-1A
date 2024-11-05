% fonction maximisation_classification_SVM_noyau (pour l'exercice 2)

function [pourcentage_meilleur_classification_SVM_test, sigma_opt_test, vecteur_pourcentages_bonnes_classifications_SVM_app, vecteur_pourcentages_bonnes_classifications_SVM_test] = maximisation_classification_SVM_noyau(X_app, Y_app, X_test, Y_test, vecteur_sigma)

    nb_sigma = length(vecteur_sigma);

    vecteur_pourcentages_bonnes_classifications_SVM_app = zeros(nb_sigma, 1);
    vecteur_pourcentages_bonnes_classifications_SVM_test = zeros(nb_sigma, 1);

    pourcentage_meilleur_classification_SVM_test = 0;

    for i = 1:nb_sigma
        sigma = vecteur_sigma(i);

        % estimation des paramètres sur l'ensemble d'apprentissage
        [X_VS, Y_VS, Alpha_VS, c, ~] = estim_param_SVM_noyau(X_app, Y_app, sigma);

        % prédiction et vérif sur l'ensemble d'apprentissage
        Y_app_pred = classification_SVM_avec_noyau(X_app, sigma, X_VS, Y_VS, Alpha_VS, c);
        Y_app_pred_exacte = Y_app_pred(Y_app_pred == Y_app);
        pourcentage_exact = length(Y_app_pred_exacte) / length(Y_app) * 100;
        vecteur_pourcentages_bonnes_classifications_SVM_app(i) = pourcentage_exact;

        % prédiction et vérif sur l'ensemble de test
        Y_test_pred = classification_SVM_avec_noyau(X_test, sigma, X_VS, Y_VS, Alpha_VS, c);
        Y_test_pred_exacte = Y_test_pred(Y_test_pred == Y_test);
        pourcentage_exact = length(Y_test_pred_exacte) / length(Y_test) * 100;
        vecteur_pourcentages_bonnes_classifications_SVM_test(i) = pourcentage_exact;

        % Mise à jour du meilleur pourcentage et meilleur sigma
        if pourcentage_exact > pourcentage_meilleur_classification_SVM_test
            pourcentage_meilleur_classification_SVM_test = pourcentage_exact;
            sigma_opt_test = sigma;
        end

    end

end
