import java.util.*;
import java.util.stream.Collectors;
import java.io.*;
import java.util.zip.*;
import java.time.LocalDateTime;

/**
 * La classe principale.
 *
 * Tous les traitements demandés sont faits dans la mêthode
 * {@code repondreQuestions}.
 * Il serait plus logique d'écrire des méthodes qui pemettraient d'améliorer
 * la structuration et la réutilisation. Cependant l'objectif est ici la
 * manipulation de l'API des collections, pas la structuration du code
 * en sous-programmes.
 */

public class Main {

	private static void repondreQuestions(Reader in) {
		Iterable<PointDeVente> iterable = PointDeVenteUtils.fromXML(in);

		// Construire un tableau associatif (pdvs) des points de vente avec un
		// accès par identifiant
		Map<Long, PointDeVente> pdvs = new HashMap<>();
		for (PointDeVente pdv : iterable) {
			long identifiant = pdv.getIdentifiant();
			pdvs.put(identifiant, pdv);
		}

		Set<Long> identifiants = pdvs.keySet();

		// Nombre de point de vente
		int nbPDV = pdvs.size();
		System.out.println("Nombre de point de vente : " + nbPDV);

		// Afficher le nombre de services existants
		int nbServicesExistants = 0;
		for (long identifiant : identifiants) {
			PointDeVente pdv = pdvs.get(identifiant);
			nbServicesExistants += pdv.getServices().size();
		}
		System.out.println("Nombre de services existants : " + nbServicesExistants);

		// Afficher les services fournis
		for (long identifiant : identifiants) {
			PointDeVente pdv = pdvs.get(identifiant);
			System.out.println(pdv.getServices());
		}

		// Afficher la ville correspondant au point de vente d'identifiant
		// 31075001 et le prix du gazole le 25 janvier 2017 à 10h.
		long id = 31075001;
		PointDeVente pdvId = pdvs.get(id);
		System.out.println("Ville : " + pdvId.getVille());
		Carburant carburant = Carburant.GAZOLE;
		LocalDateTime date = LocalDateTime.parse("2017-01-25T10:00:00");
		System.out.println("Prix du gazole : " + pdvId.getPrix(carburant, date));

		// Afficher le nombre de villes offrant au moins un point de vente
		Set<String> villes = new HashSet<String>();
		for (long identifiant : identifiants) {
			PointDeVente pdv = pdvs.get(identifiant);
			villes.add(pdv.getVille());
		}
		int nbVilles = villes.size();
		System.out.println("Nombre de villes offrant au moins un point de vente : " + nbVilles);

		// Afficher le nombre moyen de points de vente par ville
		System.out.println("Nombre moyen de point de vente par ville : " + nbPDV / nbVilles);

		// le nombre de villes offrants un certain nombre de carburants
		// tOdO

		// Afficher le nombre et les points de vente dont le code postal est 31200
		int nbPDVPostal = 0;
		for (long identifiant : identifiants) {
			PointDeVente pdv = pdvs.get(identifiant);
			if (pdv.getCodePostal().equals("31200")) {
				System.out.println(pdv);
				nbPDVPostal++;
			}
		}
		System.out.println("Le nombre de point de vente dont le code postal est \"31200\" est : " + nbPDVPostal);

		// Nombre de PDV de la ville de Toulouse qui proposent et du Gazole
		// et du GPLc.
		LocalDateTime maintenant = LocalDateTime.now();
		int nbPDVToulouse = 0;
		for (long identifiant : identifiants) {
			PointDeVente pdv = pdvs.get(identifiant);
			Boolean bonneVille = pdv.getVille().equals("Toulouse");
			Boolean bonsCarburants = pdv.getPrix(Carburant.GAZOLE, maintenant) * pdv.getPrix(Carburant.GPLc, maintenant) > 0;
			if (bonneVille && bonsCarburants) {
				nbPDVToulouse++;
			}
		}
		System.out.println("Le nombre de PDV de Toulouse qui proposent blablabla est : " + nbPDVToulouse);

		// Afficher le nom et le nombre de points de vente des villes qui ont au
		// moins 20 points de vente
	}

	private static Reader ouvrir(String nomFichier)
			throws FileNotFoundException, IOException {
		if (nomFichier.endsWith(".zip")) {
			// On suppose que l'archive est bien formée :
			// elle contient fichier XML !
			ZipFile zfile = new ZipFile(nomFichier);
			ZipEntry premiere = zfile.entries().nextElement();
			return new InputStreamReader(zfile.getInputStream(premiere));
		} else {
			return new FileReader(nomFichier);
		}
	}

	public static void main(String[] args) {
		if (args.length != 1) {
			System.out.println("usage : java Main <fichier.xml ou fichier.zip>");
		} else {
			try (Reader in = ouvrir(args[0])) {
				repondreQuestions(in);
			} catch (FileNotFoundException e) {
				System.out.println("Fichier non trouvé : " + args[0]);
			} catch (Exception e) {
				System.out.println(e.getMessage());
				e.printStackTrace();
			}
		}
	}

}
