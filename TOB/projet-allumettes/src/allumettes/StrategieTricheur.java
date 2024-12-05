package allumettes;

/**
 * Stratégie tricheur.
 * Description : retirer directement toutes les allumettes sauf 2, puis en
 * prendre 1.
 */
public class StrategieTricheur implements Strategie {

    /**
     * Obtenir le nombre d'allumettes à prendre avec la stratégie tricheur.
     *
     * @param jeu le Jeu en cours
     * @return le nombre d'allumettes souhaité
     */
    public int getPrise(Jeu jeu) {
        System.out.println("[Je triche...]");
        try {
            jeu.retirer(jeu.getNombreAllumettes() - 2);
        } catch (CoupInvalideException e) {
            // pas sensé arriver
        }
        System.out.println("[Allumettes restantes : " + jeu.getNombreAllumettes() + "]");

        int nbPrise = 1;
        return nbPrise;
    }

}
