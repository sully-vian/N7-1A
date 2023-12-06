% fonction classification_SVM (pour l'exercice 1)

function Y_pred = classification_SVM(X,w,c)

    N = length(X);

    Y_pred = zeros(N,1);
   
    % Calcul de Y_pred élément par élémént
    for i = 1:N
        x = X(i,:)';
        Y_pred(i) = sign(w' * x - c);
    end

end