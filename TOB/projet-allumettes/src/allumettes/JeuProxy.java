package allumettes;

/**
 * Sert de procuration pour le jeu réel.
 * Le joueur y a accès.
 *
 * @author Vianney Hervy
 */
public class JeuProxy implements Jeu {

    /**
     * Jeu réel dont le proxy est la procuration.
     */
    private Jeu jeuReel;

    /**
     * Créer un Jeu procuration de jeuReel.
     *
     * @param jeuReel Jeu dont on veut la procuration
     */
    public JeuProxy(Jeu jeuReel) {
        this.jeuReel = jeuReel;
    }

    /**
     * Obtenir le nombre d'allumettes encore en jeu.
     *
     * @return nombre d'allumettes encore en jeu
     */
    @Override
    public int getNombreAllumettes() {
        return this.jeuReel.getNombreAllumettes();
    }

    /**
     * Retirer des allumettes. Si le joueur appelle cette méthode, la triche est
     * dénoncée à l'arbitre.
     *
     * @param nbPrise nombre d'allumettes prises.
     * @throws CoupInvalideException tentative de triche
     */
    @Override
    public void retirer(int nbPrise) throws CoupInvalideException {
        throw new OperationInterditeException(nbPrise);
    }
}
