package allumettes;

import java.util.Scanner;

/**
 * Stratégie Humain.
 * Description : demander à l'utilisateur le nombe d'allumettes à prendre.
 */
public class StrategieHumain implements Strategie {

    /**
     * Scanner les entrées utilisateur.
     */
    private Scanner scan;

    /**
     * Nom du joueur adoptant cette stratégie.
     */
    private String nomJoueur;

    /**
     * Construire une Stratégie avec un joueur de nom nomJoueur.
     *
     * @param nomJoueur le nom du joueur adoptant cette stratégie
     * @param scan      le scanner qui permet de lire l'entrée utilisateur
     */
    public StrategieHumain(String nomJoueur, Scanner scan) {
        this.nomJoueur = nomJoueur;
        this.scan = scan;
    }

    /**
     * Demander à l'utilisateur le nombre d'allumettes qu'il souhaite prendre.
     *
     * @param jeu le Jeu en cours
     * @return le nombre d'allumettes que le joueur souhaite prendre
     */
    public int getPrise(Jeu jeu) {

        Boolean formatValide = false;
        Boolean triche = false;
        String entree = null;

        while (!formatValide || triche) {
            System.out.print(this.nomJoueur + ", combien d'allumettes ? ");
            entree = this.scan.next();

            if (entree.equals("triche")) {
                try {
                    jeu.retirer(1);
                } catch (CoupInvalideException e) {
                    // pas sensé arriver
                }
                System.out.println("[Une allumette en moins, plus que "
                        + jeu.getNombreAllumettes() + ". Chut !]");
                triche = true;

            } else if (!entree.matches("-?[0-9]+")) {
                System.out.println("Vous devez donner un entier.");

            } else {
                formatValide = true;
                triche = false;
            }
        }

        int nbPrise = Integer.parseInt(entree);

        return nbPrise;
    }

}
