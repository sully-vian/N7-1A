package allumettes;

/**
 * SujetReel auquel on accède par le biais d'un JeuProxy.
 * Le Joueur n'y a pas accès, l'arbitre si.
 *
 * @author Vianney Hervy
 */
public class JeuReel implements Jeu {

    /**
     * Nombre d'allumettes restantes dans le jeu.
     */
    private int nombreAllumettes;

    /**
     * Construire un JeuReel avec un nombre initial fixé d'allumettes.
     *
     * @param nombreAllumettesInitial nombre initial d'allumettes
     */
    JeuReel(int nombreAllumettesInitial) {
        this.nombreAllumettes = nombreAllumettesInitial;
    }

    /**
     * Obtenir le nombre d'allumettes encore en jeu.
     *
     * @return nombre d'allumettes encore en jeu
     */
    public int getNombreAllumettes() {
        return this.nombreAllumettes;
    }

    /**
     * Retirer des allumettes. Le nombre d'allumettes doit être compris
     * entre 1 et PRISE_MAX, dans la limite du nombre d'allumettes encore
     * en jeu.
     *
     * @param nbPrise nombre d'allumettes prises.
     * @throws CoupInvalideException tentative de prendre un nombre invalide
     *                               d'allumettes
     */
    public void retirer(int nbPrise) throws CoupInvalideException {
        if (nbPrise > this.getNombreAllumettes()) {
            throw new CoupInvalideException(nbPrise, "> " + this.getNombreAllumettes());
        }

        this.nombreAllumettes -= nbPrise;
    }
}
