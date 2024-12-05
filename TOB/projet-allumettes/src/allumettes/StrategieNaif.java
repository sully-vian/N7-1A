package allumettes;

import java.util.Random;

/**
 * Stratégie naïve.
 * Description : prendre un nombre aléatoire d'allumettes entre 1 et PRISE_MAX.
 */
public class StrategieNaif implements Strategie {

    /**
     * Obtenir le nombre d'allumettes à prendre avec la stratégie naïve.
     *
     * @param jeu le Jeu en cours
     * @return le nombre d'allumettes souhaitées
     */
    public int getPrise(Jeu jeu) {
        int nbPrise = (new Random()).nextInt(Jeu.PRISE_MAX) + 1;
        return nbPrise;
    }
}
