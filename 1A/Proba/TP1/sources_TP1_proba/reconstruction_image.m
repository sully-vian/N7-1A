% Fonction reconstruction_image (exercice_3.m)

function I_reconstruite = reconstruction_image(I_encodee, dictionnaire, hauteur_I, largeur_I)

    I_decodee = huffmandeco(I_encodee, dictionnaire);

    I_decorrelee = reshape(I_decodee, hauteur_I, largeur_I);

    I_reconstruite = I_decorrelee;

    for j = 2:largeur_I
        I_reconstruite(:, j) = I_decorrelee(:, j) + I_reconstruite(:, j - 1);
    end

end
