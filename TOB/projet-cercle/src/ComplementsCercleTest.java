import org.junit.*;
import static org.junit.Assert.*;

/**
 * L'objectif de cette classe est de vérifier (en profondeur) que la classe
 * Cercle a été
 * correctement programmée et que je sais écrire des tests utilisant JUnit4.
 * 
 * @author Vianney
 * @version 1.0
 */

public class ComplementsCercleTest {

    // précision pour les comparaisons réelles
    public static final double EPSILON = 0.001;

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
    public void testerDoubleTranslation() {
        Point A = new Point(0, 0);
        Cercle C1 = new Cercle(A, 5);
        Cercle C2 = new Cercle(A, 10);
        A.translater(0, 1);
        C1.translater(1, 0);
        C2.translater(1, 1);
        
        memesCoordonnees(null, new Point(0,1), A);
        memesCoordonnees(null, new Point(1,0), C1.getCentre());
        memesCoordonnees(null, new Point(1,1), C2.getCentre());
    }
}