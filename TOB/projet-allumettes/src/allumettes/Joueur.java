package allumettes;

/**
 * Joueur de jeu d'allumettes.
 * On peut demander à un Joueur son nom et le nombre d'allumettes qu'il souhaite
 * prendre.
 *
 * @author Vianney Hervy
 */
public class Joueur {

    /**
     * Le nom du joueur.
     */
    private String nom;

    /**
     * Stratégie du joueur.
     */
    private Strategie strategie;

    /**
     * Construire un Joueur à partir de son nom.
     *
     * @param nom le nom du joueur
     * @param strategie la stratégie du joueur
     */
    public Joueur(String nom, Strategie strategie) {
        this.nom = nom;
        this.strategie = strategie;
    }

    /**
     * Obtenir le nom du joueur.
     *
     * @return le nom du joueur
     */
    public String getNom() {
        return this.nom;
    }

    /**
     * Demander à un Joueur le nombre d'allumettes qu'il souhaite prendre.
     * La réponse dépend de la stratégie du joueur.
     *
     * @param jeu le jeu en cours
     * @return nombre d'allumettes que le joueur souhaite prendre
     * @throws CoupInvalideException si le coup est invalide
     */
    public int getPrise(Jeu jeu) throws CoupInvalideException {
        int nbPrise = this.strategie.getPrise(jeu);
        return nbPrise;
    };

}
