% Fonction ecriture_RVB

function image_RVB = ecriture_RVB(image_originale)

    canal_R = image_originale(1:2:end, 2:2:end);
    canal_B = image_originale(2:2:end, 1:2:end);
    canal_V = (image_originale(1:2:end, 1:2:end) + image_originale(2:2:end, 2:2:end)) / 2;

    image_RVB = cat(3, canal_R, canal_V, canal_B);

end
