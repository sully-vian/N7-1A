% Fonction ensemble_E_recursif (exercie_1.m)

function [E, contour, G_somme] = ensemble_E_recursif(E, contour, G_somme, i, j, voisins, G_x, G_y, card_max, cos_alpha)
    contour(i, j) = 0;
    k = 1;
    nb_voisin = 8;

    while (k <= nb_voisin) && (size(E, 1) < card_max)

        % prendre voisin de k
        i_voisin = i + voisins(k, 1);
        j_voisin = j + voisins(k, 2);
        k = k + 1;

        [Ajoutable, G_voisin] = ajoutable(contour, i_voisin, j_voisin, G_x, G_y, G_somme, cos_alpha);

        if Ajoutable
            E = [E; i, j];
            G_somme = G_somme + G_voisin;
            [E, contour, G_somme] = ensemble_E_recursif(E, contour, G_somme, i_voisin, j_voisin, voisins, G_x, G_y, card_max, cos_alpha);
        end

    end

end

function [Ajoutable, G_voisin] = ajoutable(contour, i_voisin, j_voisin, G_x, G_y, G_somme, cos_alpha)

    if contour(i_voisin, j_voisin)
        G_voisin = [G_x(i_voisin, j_voisin), G_y(i_voisin, j_voisin)];
        prod_scal = (G_voisin / norm(G_voisin)) * (G_somme / norm(G_somme))';
        Gradients_alignes = (prod_scal > cos_alpha);

        Ajoutable = contour(i_voisin, j_voisin) && Gradients_alignes;
    else
        G_voisin = [0, 0];
        Ajoutable = false;
    end

end
