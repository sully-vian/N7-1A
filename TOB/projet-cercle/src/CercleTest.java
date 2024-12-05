import java.awt.Color;
import org.junit.*;
import static org.junit.Assert.*;

/**
 * L'objectif de cette classe est de vérifier que la classe Cercle a été
 * correctement programmée et que je sais écrire des tests utilisant JUnit4.
 *
 * Les méthodes commentées sont responsables des tests de robustesse. Elles
 * doivent être utilisées avec l'option -ea.
 *
 * @author Vianney
 * @version 1.0
 */

public class CercleTest {

    // précision pour les comparaisons réelles
    public static final double EPSILON = 0.001;

    // Les points du sujet
    private Point C, D, E;

    // Les cercles du sujet
    private Cercle C2, C3;

    @Before
    public void setUp() {
        // Construire les points
        C = new Point(4, 1);
        D = new Point(8, 1);
        E = new Point(8, 4);

        // Construire les cercles
        C2 = new Cercle(new Point(6, 1), 2);
        C3 = Cercle.creerCercle(D, E);
    }

    /**
     * Vérifier si deux points ont mêmes coordonnées.
     * 
     * @param p1 le premier point
     * @param p2 le deuxième point
     */
    private static void memesCoordonnees(String message, Point p1, Point p2) {
        assertEquals(message + " (x)", p1.getX(), p2.getX(), EPSILON);
        assertEquals(message + " (y)", p1.getY(), p2.getY(), EPSILON);
    }

    @Test
    public void testerE12() {
        // E12:
        // On peut construire un cercle à partir de deux points diamétralement opposés.
        // Sa couleur est considérée comme étant le bleu. Par exemple, le cercle C2 est
        // construit à partir des deux points C et D.
        memesCoordonnees(null, new Point(6, 1), C2.getCentre());
        assertEquals(C.distance(D), C2.getDiametre(), EPSILON);
        assertEquals(Color.blue, C2.getCouleur());
    }

    // @Test(expected = AssertionError.class)
    // public void testerE12Bis() {
    //     Cercle CTemp = new Cercle(C, C);
    //     CTemp.translater(0, 0);
    // }

    @Test
    public void testerE13() {
        // E13:
        // On peut construire un cercle à partir de deux points diamétralement opposés
        // et de sa couleur
        Cercle C4 = new Cercle(D, E, Color.green);
        memesCoordonnees(null, new Point(8, 2.5), C4.getCentre());
        assertEquals(D.distance(E), C4.getDiametre(), EPSILON);
        assertEquals(Color.green, C4.getCouleur());
    }

    // @Test(expected = AssertionError.class)
    // public void testerE13Bis() {
    //     Cercle CTemp = new Cercle(C, C, Color.red);
    //     CTemp.translater(0, 0);
    // }

    // @Test(expected = AssertionError.class)
    // public void testerE13Ter() {
    //     Cercle CTemp = new Cercle(C, D, null);
    //     CTemp.translater(0, 0);
    // }

    @Test
    public void testerE14() {
        // E14:
        // Une méthode de classe creerCercle(Point, Point) permet de créer un cercle à
        // partir de deux points, le premier correspond au centre du cercle et le
        // deuxième est un point du cercle (de sa circonférence). Ces deux points
        // forment donc un rayon du cercle. Par exemple, le cercle C3 est construit à
        // partir des points D (centre) et E (circonférence). Le cercle est bleu.
        memesCoordonnees(null, D, C3.getCentre());
        assertEquals(D.distance(E), C3.getRayon(), EPSILON);
        assertEquals(Color.blue, C3.getCouleur());
    }

    // @Test(expected = AssertionError.class)
    // public void testerE14Bis() {
    //     Cercle CTemp = Cercle.creerCercle(C, C);
    //     CTemp.translater(0, 0);
    // }

}