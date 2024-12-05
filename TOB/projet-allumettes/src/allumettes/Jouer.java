package allumettes;

import java.util.Scanner;

/**
 * Lance une partie des 13 allumettes en fonction des arguments fournis
 * sur la ligne de commande.
 *
 * @author Xavier Crégut
 * @version $Revision: 1.5 $
 */
public final class Jouer {

	/**
	 * Nombre initial d'allumettes dans le jeu.
	 */
	private static final int NOMBREALLUMETTESINITIAL = 13;

	/**
	 * Scanner global du jeu utilisé dans la stratégie humain.
	 */
	private static final Scanner SCANNER = new Scanner(System.in);

	/**
	 * Lancer une partie. En argument sont donnés les deux joueurs sous
	 * la forme nom@stratégie.
	 *
	 * @param args la description des deux joueurs
	 */
	public static void main(String[] args) {

		try {
			verifierNombreArguments(args);

			boolean confiant = confiance(args);

			// Créer les personnages
			Joueur j1 = creerJoueur(args[args.length - 2]);
			Joueur j2 = creerJoueur(args[args.length - 1]);
			Arbitre arbitre = new Arbitre(confiant, j1, j2);

			// Jouer la partie
			Jeu jeu = new JeuReel(NOMBREALLUMETTESINITIAL);
			arbitre.arbitrer(jeu);

		} catch (ConfigurationException e) {
			System.out.println();
			System.out.println("Erreur : " + e.getMessage());
			afficherUsage();
			System.exit(1);
		}
	}

	/**
	 * Vérifier que le nombre d'arguments passés en ligne de commande correspond à
	 * l'usage de l'application.
	 *
	 * @param args arguments passés en ligne de commande
	 */
	public static void verifierNombreArguments(String[] args) {
		final int nbJoueurs = 2;
		if (args.length < nbJoueurs) {
			throw new ConfigurationException("Trop peu d'arguments : "
					+ args.length);
		}
		if (args.length > nbJoueurs + 1) {
			throw new ConfigurationException("Trop d'arguments : "
					+ args.length);
		}
	}

	/**
	 * Afficher des indications sur la manière d'exécuter cette classe.
	 */
	public static void afficherUsage() {
		System.out.println("\n" + "Usage :"
				+ "\n\t" + "java allumettes.Jouer joueur1 joueur2"
				+ "\n\t\t" + "joueur est de la forme nom@stratégie"
				+ "\n\t\t" + "strategie = naif | rapide | expert | humain | tricheur"
				+ "\n"
				+ "\n\t" + "Exemple :"
				+ "\n\t" + "	java allumettes.Jouer Xavier@humain "
				+ "Ordinateur@naif"
				+ "\n");
	}

	/**
	 * Interpréter le paramètre de confiance.
	 *
	 * @param args arguments passés en ligne de commande
	 * @return true si l'arbitre sera confiant
	 * @throws ConfigurationException levée lorsque l'option utilisée n'existe pas
	 */
	public static Boolean confiance(String[] args) throws ConfigurationException {
		Boolean confiant;
		if (args.length == 2) {
			confiant = false;
		} else if (args[0].equals("-confiant")) {
			confiant = true;
		} else {
			throw new ConfigurationException("Cette option n'existe pas : " + args[0]);
		}
		return confiant;
	}

	/**
	 * Créer un Joueur depuis les arguments en ligne de commande.
	 *
	 * @param arg description du joueur avec le format nom@stratégie
	 * @return Joueur avec les attributs souhaités
	 */
	public static Joueur creerJoueur(String arg) throws ConfigurationException {

		String[] nomStrategie = arg.split("@");
		if (nomStrategie.length != 2) {
			throw new ConfigurationException("Format invalide pour décrire un joueur : "
					+ arg);
		}

		String nom = nomStrategie[0];
		String strategie = nomStrategie[1];
		Joueur j;

		switch (strategie) {
			case "naif":
				j = new Joueur(nom, new StrategieNaif());
				break;

			case "rapide":
				j = new Joueur(nom, new StrategieRapide());
				break;

			case "expert":
				j = new Joueur(nom, new StrategieExpert());
				break;

			case "humain":
				j = new Joueur(nom, new StrategieHumain(nom, Jouer.SCANNER));
				break;

			case "tricheur":
				j = new Joueur(nom, new StrategieTricheur());
				break;

			default:
				throw new ConfigurationException("Stratégie inconnue : " + strategie);
		}

		return j;
	}

	/**
	 * Constructeur privé pour empêcher l'instanciation de cette classe.
	 * Il permet de vérifier la règle checkstyle HideUtilityClassConstructor
	 */
	private Jouer() {
	}

}
