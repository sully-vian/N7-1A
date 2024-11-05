% fonction moyenne_normalisee_3v (pour l'exercice 1bis)

function x = moyenne_normalisee_3v(I)

    % Conversion en flottants :
    I = single(I);
    
    % Calcul des couleurs normalisees :
    somme_canaux = max(1,sum(I,3));
    r = I(:,:,1) ./ somme_canaux;
    v = I(:,:,2) ./ somme_canaux;
    b = I(:,:,3) ./ somme_canaux;
    
    % Calcul des couleurs moyennes :
    r_barre = mean(r(:));
    v_barre = mean(v(:));

    s = size(r);
    s1 = s(1);
    s2 = s(2);
    k1 = 100; % largeur pourtour
    k2 = 15; % largeur centre
    rayon = 10; % rayon du centre

    % coordonnées centre
    c1 = round(s1/2);
    c2 = round(s2/2);


    % calcul moyenne pourtour avec masque
    Masque1 = ones(s1,s2);
    Masque1(k1+1:end-k1, k1+1:end-k1) = 0; % des 0 au centre

    % calcul moyenne centre avec masque
    Masque2 = zeros(s1,s2);
    Masque2(c1-k2:c1+k2, c2-k2:c2+k2) = 1; % des 1 sur un carré au centre

    % calcul moyenne centre avec masque rond
    Masque_rond = zeros(s1,s2);
    for i = 1:s1
        for j = 1:s2
            if (c1-i)^2 + (c2-j)^2 <= rayon^2
                Masque_rond(i,j) = 1; % des 1 sur un rond au centre
            end
        end
    end
    
    % on garde là où ya des 1
    rp = r(Masque1 == 1);
    rc = r(Masque2 == 1);
    % rc = r(Masque_rond == 1);

    rp_mean = mean(rp);
    rc_mean = mean(rc);

    carac3 = rp_mean - rc_mean;

    % concaténation
    x = [r_barre v_barre carac3];

end
