package allumettes;

/**
 * Classe Arbitre
 * Un arbitre fait respecter les règles du jeu aux deux joueurs.
 *
 * @author Vianney HERVY
 */
public class Arbitre {

    /**
     * Premier joueur affrontant le second.
     */
    private Joueur j1;

    /**
     * Second joueur affrontant le premier.
     */
    private Joueur j2;

    /**
     * Confiance de l'arbitre.
     * Si confiant, alors l'arbitre ne détecte pas la triche.
     */
    private Boolean confiant;

        /**
     * Créer un arbitre (confiant ou non) faisant respecter les règles aux deux
     * joueurs fournis.
     *
     * @param j1       joueur 1
     * @param j2       joueur 2
     * @param confiant confiance de l'arbitre en les joueurs (détection ou non de la
     *                 triche)
     */
    public Arbitre(Boolean confiant, Joueur j1, Joueur j2) {
        this.j1 = j1;
        this.j2 = j2;
        this.confiant = confiant;
    }

    /**
     * Créer un arbitre faisant respecter les règles aux deux joueurs fournis.
     * Il détecte la triche par défaut
     *
     * @param j1 joueur 1
     * @param j2 joueur 2
     */
    public Arbitre(Joueur j1, Joueur j2) {
        this(false, j1, j2);
    }

    /**
     * Arbitrer une partie d'un jeu.
     *
     * @param jeu jeu à arbitrer
     */
    public void arbitrer(Jeu jeu) {

        Jeu jeuFourni = confiant ? jeu : new JeuProxy(jeu); // jeu fourni au joueur

        Boolean arreter = false; // arrêter la partie

        Joueur joueurActuel = this.j1;
        int nbAlumettes = jeu.getNombreAllumettes();

        while ((nbAlumettes != 0) && !arreter) {

            try {

                jouerTour(jeu, jeuFourni, nbAlumettes, joueurActuel);

                System.out.println();
                joueurActuel = joueurSuivant(joueurActuel); // changement de joueur

            } catch (OperationInterditeException e) {
                System.out.println("Abandon de la partie car " + joueurActuel.getNom()
                        + " triche !");
                arreter = true;

            } catch (CoupInvalideException e) {
                System.out.println("Impossible ! Nombre invalide : "
                        + e.getCoup() + " (" + e.getProbleme() + ")");

            } finally {
                nbAlumettes = jeu.getNombreAllumettes(); // m-à-j du nombre d'allumettes
            }
        }

        if (!arreter) {
            Joueur joueurPerdant = joueurSuivant(joueurActuel);
            annoncerResultats(joueurPerdant);
        }
    }

    /**
     * Jouer un tour de jeu.
     * Cette méthode n'existe que pour raccourcir la méthode arbitrer.
     *
     * @param jeu          jeu en cours
     * @param jeuFourni    jeu fourni au joueur
     * @param nbAlumettes  nombre d'allumettes restantes
     * @param joueurActuel joueur jouant à ce tour
     * @throws CoupInvalideException levée si le coup est invalide
     */
    public void jouerTour(Jeu jeu, Jeu jeuFourni, int nbAlumettes, Joueur joueurActuel)
            throws CoupInvalideException {

        System.out.println("Allumettes restantes : " + nbAlumettes);

        int nbPrise = joueurActuel.getPrise(jeuFourni);

        String pluriel = (nbPrise > 1) ? "s" : "";
        System.out.println(joueurActuel.getNom() + " prend " + nbPrise
                + " allumette" + pluriel + ".");

        int priseMax = Math.min(nbAlumettes, Jeu.PRISE_MAX);
        if (!coupValide(nbPrise, priseMax)) {
            String comparaison = (nbPrise > priseMax) ? "> " + priseMax : "< 1";
            throw new CoupInvalideException(nbPrise, comparaison);
        }

        jeu.retirer(nbPrise);
    }

    /**
     * Vérifier si un coup est valide ou non.
     *
     * @param nbPrise  nombre d'allumettes souhaitées
     * @param priseMax nombre maximal d'allumettes que l'on peut prendre
     * @return validité du coup
     */
    public Boolean coupValide(int nbPrise, int priseMax) {
        Boolean valide;
        if (nbPrise > priseMax) {
            valide = false;
        } else if (nbPrise < 1) {
            valide = false;
        } else {
            valide = true;
        }
        return valide;
    }

    /**
     * Annoncer les résultats de la partie.
     *
     * @param joueurPerdant Le joueur perdant
     */
    public void annoncerResultats(Joueur joueurPerdant) {
        System.out.println(joueurPerdant.getNom() + " perd !");
        Joueur joueurGagant = joueurSuivant(joueurPerdant);
        System.out.println(joueurGagant.getNom() + " gagne !");
    }

    /**
     * Renvoyer le joueur jouant le prochain coup.
     *
     * @param joueurActuel joueur qui vient de finir son tour
     * @return joueur suivant
     */
    public Joueur joueurSuivant(Joueur joueurActuel) {
        return (joueurActuel == this.j1) ? this.j2 : this.j1;
    }

}
