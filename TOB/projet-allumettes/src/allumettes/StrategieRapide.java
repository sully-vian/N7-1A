package allumettes;

/**
 * Stratégie rapide.
 * Description : prendre le maximum d'allumettes possible.
 */
public class StrategieRapide implements Strategie {

    /**
     * Obtenir le nombre d'allumettes à prendre avec a strétgie rapide.
     *
     * @param jeu le Jeu en cours
     * @return le nombre d'allumettes souhaitées
     */
    public int getPrise(Jeu jeu) {
        int nbPrise = Math.min(Jeu.PRISE_MAX, jeu.getNombreAllumettes());
        return nbPrise;
    }

}
