package allumettes;

public interface Strategie {

    /**
     * Obtenir le nombre d'allumettes souhaité en fonction de la stratégie.
     *
     * @param jeu le jeu en cours
     * @return le nombre d'allumettes souhaité
     */
    int getPrise(Jeu jeu);

}
