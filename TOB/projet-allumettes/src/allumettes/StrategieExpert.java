package allumettes;

/**
 * Stratégie experte.
 * Description : laisser un nombre d'allumettes congru à 1 modulo PRISE_MAX si
 * possible, sinon, en prendre 1 (afin de faire durer la partie le plus
 * longtemps possible et espérer une erreur de l'aadversaire.)
 */
public class StrategieExpert implements Strategie {

    /**
     * Obtenir le nombre d'allumettes à prendre avec la strétgie experte.
     *
     * @param jeu le jeu en cours
     * @return le nombre d'allumettes souhaité
     */
    public int getPrise(Jeu jeu) {
        int mod = Jeu.PRISE_MAX + 1;

        int reste = jeu.getNombreAllumettes() % mod;

        int nbPrise = (reste == 1) ? 1 : (reste + Jeu.PRISE_MAX) % mod;
        return nbPrise;
    }

}
