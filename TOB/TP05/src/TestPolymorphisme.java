/**
 * Tester le polymorphisme (principe de substitution) et la liaison
 * dynamique.
 * 
 * @author Xavier Crégut
 * @version 1.5
 */
public class TestPolymorphisme {

	/** Méthode principale */
	public static void main(String[] args) {
		// Créer et afficher un point p1
		Point p1 = new Point(3, 4); // Est-ce autorisé ? Pourquoi ?
		// Oui, la classe Point existe encore et est accessible
		p1.translater(10, 10); // Quel est le translater exécuté ? Celui défini dans Point.java
		System.out.print("p1 = ");
		p1.afficher();
		System.out.println(); // Qu'est ce qui est affiché ? "p1 = (13, 14)"

		// Créer et afficher un point nommé pn1
		PointNomme pn1 = new PointNomme(30, 40, "PN1"); // Est-ce autorisé ? Pourquoi ? Oui, même raison
		pn1.translater(10, 10); // Quel est le translater exécuté ?
		// Celui défini dans Point.java car PointNomme étend cette classe
		System.out.print("pn1 = ");
		pn1.afficher();
		System.out.println(); // Qu'est ce qui est affiché ?
		// "pn1 : PN1:(40, 50)" PointNomme.afficher remplace Point.afficher

		// Définir une poignée sur un point
		Point q;

		// Attacher un point à q et l'afficher
		q = p1; // Est-ce autorisé ? Pourquoi ? Oui parce que les deux sont de même classe
		System.out.println("> q = p1;");
		System.out.print("q = ");
		q.afficher();
		System.out.println(); // Qu'est ce qui est affiché ? "q = (13, 14)"

		// Attacher un point nommé à q et l'afficher
		System.out.println("type(q) = " + q.getClass().getName());
		q = pn1; // Est-ce autorisé ? Pourquoi ?
		// le type de q est alors étendu à PointNomme
		System.out.println("> q = pn1;");
		System.out.println("type(q) = " + q.getClass().getName());
		System.out.print("q = ");
		q.afficher();
		System.out.println(); // Qu'est ce qui est affiché ? "q = (40, 50)"

		// Définir une poignée sur un point nommé
		PointNomme qn;

		// // Attacher un point à q et l'afficher
		// qn = p1; // Est-ce autorisé ? Pourquoi ?
		// // Non, conversion impossible de Point à PointNomme
		// System.out.println("> qn = p1;");
		// System.out.print("qn = ");
		// // qn.afficher();
		// System.out.println(); // Qu'est ce qui est affiché ? Rien, erreur

		// Attacher un point nommé à qn et l'afficher
		qn = pn1; // Est-ce autorisé ? Pourquoi ? Oui car même type
		System.out.println("> qn = pn1;");
		System.out.print("qn = ");
		qn.afficher();
		System.out.println();
		// Qu'est ce qui est affiché ? "qn = PN1:(40, 50)"

		double d1 = p1.distance(pn1); // Est-ce autorisé ? Pourquoi ?
		// Oui car méthode du type parent applicable au type enfant
		System.out.println("distance = " + d1);

		double d2 = pn1.distance(p1); // Est-ce autorisé ? Pourquoi ?
		// Oui car méthode partagée par les deux types
		System.out.println("distance = " + d2);

		double d3 = pn1.distance(pn1); // Est-ce autorisé ? Pourquoi ?
		// Oui car même type
		System.out.println("distance = " + d3);

		// System.out.println("> qn = q;");
		// qn = q; // Est-ce autorisé ? Pourquoi ?
		// // Non, conversion impossible de Point à PointNomme
		// System.out.print("qn = ");
		// qn.afficher();
		// System.out.println(); // Qu'est ce qui est affiché ? Rien, erreur

		System.out.println("> qn = (PointNomme) q;");
		System.out.println("type(qn) = " + qn.getClass().getName());
		System.out.println("type(q) = " + q.getClass().getName());
		qn = (PointNomme) q; // Est-ce autorisé ? Pourquoi ?
		// oui, car q est alors
		System.out.print("qn = ");
		qn.afficher();
		System.out.println();

		System.out.println("> qn = (PointNomme) p1;");
		qn = (PointNomme) p1; // Est-ce autorisé ? Pourquoi ?
		// Non, 
		System.out.print("qn = ");
		qn.afficher();
		System.out.println();
	}

}
