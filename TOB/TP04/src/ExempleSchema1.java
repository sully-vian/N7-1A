import java.awt.Color;
import afficheur.AfficheurSVG;
import afficheur.Ecran;
// import afficheur.AfficheurSVG;

/**
 * Construire le schéma proposé dans le sujet de TP avec des points,
 * et des segments.
 *
 * @author Xavier Crégut
 * @version $Revision: 1.7 $
 */
public class ExempleSchema1 {

	// Créer les trois segments
	private static Point p1 = new Point(3, 2);
	private static Point p2 = new Point(6, 9);
	private static Point p3 = new Point(11, 4);
	private static Segment s12 = new Segment(p1, p2); // vert
	private static Segment s23 = new Segment(p2, p3); // vert
	private static Segment s31 = new Segment(p3, p1); // vert

	// Créer le barycentre
	private static double sx = p1.getX() + p2.getX() + p3.getX();
	private static double sy = p1.getY() + p2.getY() + p3.getY();
	private static Point barycentre = new Point(sx / 3, sy / 3); // vert

	/**
	 * Construire le schéma et le manipuler.
	 * Le schéma est affiché.
	 * 
	 * @param args les arguments de la ligne de commande
	 */
	public static void main(String[] args) {

		// Afficher le schéma
		sujet();

		Ecran e = new Ecran("Schéma", 600, 400, 20);
		exercice2(e); // dessiner le schéma sur un écran
		exercice3(e); // dessin translaté

		AfficheurSVG svg = new AfficheurSVG("Schéma SVG", null, 600, 400);
		exercice4(svg); // affichage / écriture svg

		// affichage textuel
		AfficheurTexte texte = new AfficheurTexte();
		exercice5(texte); // affichage textuel
	}

	private static void sujet() {
		System.out.println("Le schéma est composé de : ");
		s12.afficher();
		System.out.println();
		s23.afficher();
		System.out.println();
		s31.afficher();
		System.out.println();
		barycentre.afficher();
		System.out.println();
	}

	private static void exercice2(Ecran e) {
		e.dessinerLigne(p1.getX(), p1.getY(), p2.getX(), p2.getY(), Color.green); // dessin s12
		e.dessinerLigne(p2.getX(), p2.getY(), p3.getX(), p3.getY(), Color.green); // dessin s23
		e.dessinerLigne(p3.getX(), p1.getY(), p3.getX(), p2.getY(), Color.green); // dessin s31
		e.dessinerPoint(barycentre.getX(), barycentre.getY(), Color.green); // dessin barycentre

	}

	private static void exercice3(Ecran e) {
		// translation du schéma
		s12.translater(4, -3);
		s23.translater(4, -3);
		s31.translater(4, -3);
		barycentre.translater(4, -3);

		// nouveau dessin
		e.dessinerLigne(p1.getX(), p1.getY(), p2.getX(), p2.getY(), Color.green); // dessin s12
		e.dessinerLigne(p2.getX(), p2.getY(), p3.getX(), p3.getY(), Color.green); // dessin s23
		e.dessinerLigne(p3.getX(), p1.getY(), p3.getX(), p2.getY(), Color.green); // dessin s31
		e.dessinerPoint(barycentre.getX(), barycentre.getY(), Color.green); // dessin barycentre
		// le barycentre n'a été translaté qu'une fois, pas deux contrairement aux
		// autres points qui sont chacun extrémimité de deux segments
	}

	private static void exercice4(AfficheurSVG svg) {
		svg.dessinerLigne(p1.getX(), p1.getY(), p2.getX(), p2.getY(), Color.green); // dessin s12
		svg.dessinerLigne(p2.getX(), p2.getY(), p3.getX(), p3.getY(), Color.green); // dessin s23
		svg.dessinerLigne(p3.getX(), p1.getY(), p3.getX(), p2.getY(), Color.green); // dessin s31
		svg.dessinerPoint(barycentre.getX(), barycentre.getY(), Color.green); // dessin barycentre
		svg.ecrire("exo4.svg");
	}

	private static void exercice5(AfficheurTexte texte) {
		texte.dessinerLigne(p1.getX(), p1.getY(), p2.getX(), p2.getY(), Color.green); // dessin s12
		texte.dessinerLigne(p2.getX(), p2.getY(), p3.getX(), p3.getY(), Color.green); // dessin s23
		texte.dessinerLigne(p3.getX(), p1.getY(), p3.getX(), p2.getY(), Color.green); // dessin s31
		texte.dessinerPoint(barycentre.getX(), barycentre.getY(), Color.green); // dessin barycentre
	}

}
