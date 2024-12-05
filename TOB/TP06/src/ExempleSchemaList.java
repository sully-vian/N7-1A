import afficheur.Ecran;
import java.util.ArrayList;
// import afficheur.Afficheur;

/** Construire le schéma proposé dans le sujet de TP avec des points,
  * des points nommés
  * et des segments.
  *
  * @author	Xavier Crégut
  * @version	$Revision: 1.7 $
  */
public class ExempleSchemaList {

	/** Construire le schéma et le manipuler.
	  * Le schéma est affiché.
	  * Ensuite, il est translaté et affiché de nouveau.
	  * @param args les arguments de la ligne de commande
	  */
	public static void main(String[] args)
	{
		// Créer les trois segments
		Point p1 = new PointNomme(3, 2, "A");
		Point p2 = new PointNomme(6, 9, "S");
		Point p3 = new Point(11, 4);
		Segment s12 = new Segment(p1, p2);
		Segment s23 = new Segment(p2, p3);
		Segment s31 = new Segment(p3, p1);

		// Créer le barycentre
		double sx = p1.getX() + p2.getX() + p3.getX();
		double sy = p1.getY() + p2.getY() + p3.getY();
		Point barycentre = new PointNomme(sx / 3, sy / 3, "C");

		// Définir le schéma (vide)
		ArrayList<ObjetGeometrique> schema = new ArrayList<ObjetGeometrique>();	// le schéma

		// Peupler le schéma
		schema.add(s12);
		schema.add(s23);
		schema.add(s31);
		schema.add(barycentre);

		// Afficher le schéma
		System.out.println("Le schéma est composé de : ");
		for (ObjetGeometrique objet : schema) {
			objet.afficher();
			System.out.println();
		}

		// Créer l'écran d'affichage
		Ecran ecran = new Ecran("ExempleSchemaTab", 600, 400, 20);
		ecran.dessinerAxes();

		// Dessiner le schéma sur l'écran graphique
		for (ObjetGeometrique objet : schema) {
			objet.dessiner(ecran);
		}

		// Translater le schéma
		System.out.println("Translater le schéma de (4, -3) : ");
		for (ObjetGeometrique objet : schema) {
			objet.translater(4, -3);
		}

		// Afficher le schéma
		System.out.println("Le schéma est composé de : ");
		for (ObjetGeometrique objet : schema) {
			objet.afficher();
			System.out.println();
		}

		// Dessiner le schéma sur l'écran graphique
		for (ObjetGeometrique objet : schema) {
			objet.dessiner(ecran);
		}

		// Forcer l'affichage du schéma (au cas où...)
		ecran.rafraichir();
	}

}