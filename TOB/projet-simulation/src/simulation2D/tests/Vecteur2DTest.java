package simulation2D.tests;

import simulation2D.physics.Vecteur2D;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

/**
 * Classe de test de la classe Vecteur2D.
 *
 * @author Yassine Bougacha & Vianney Hervy
 */
public class Vecteur2DTest {

    /**
     * La précision pour les comparaisons réelles.
     */
    private static final double EPSILON = 0.001;

    /**
     * Les vecteurs testés.
     */
    private Vecteur2D v1, v2;

    /**
     * Les vecteurs testés normalisés.
     */
    private Vecteur2D v1Normalized, v2Normalized;

    /**
     * Les scalaires choisis pour tester les opérations vecteur/scalaires.
     */
    private double a, b;

    /**
     * Initialiser les variables avant chaque test.
     */
    @Before
    public void setUp() {
        // Construire des vecteurs 2D:
        v1 = new Vecteur2D(5, 2);
        v2 = new Vecteur2D(2.4, 2.12);

        v1Normalized = v1.divide(Math.sqrt(v1.norm2()));
        v2Normalized = v2.divide(Math.sqrt(v2.norm2()));

        a = 2.5;
        b = -0.5;
    }

    /**
     * Vérifier si deux vecteurs ont les mêmes coordonnées.
     *
     * @param message le message d'erreur
     * @param vect1   le premier vecteur
     * @param vect2   le deuxiéme vecteur
     */
    static void memesCoordonnees(String message, Vecteur2D vect1, Vecteur2D vect2) {
        assertEquals(message + "(x)", vect1.getX(), vect2.getX(), EPSILON);
        assertEquals(message + "(y)", vect1.getY(), vect2.getY(), EPSILON);
    }

    /**
     * Tester la méthode getX.
     */
    @Test
    public void testGetX() {
        assertEquals("Erreur getX v1", 5, v1.getX(), EPSILON);
        assertEquals("Erreur getX v2", 2.4, v2.getX(), EPSILON);

    }

    /**
     * Tester la méthode getY.
     */
    @Test
    public void testGetY() {
        assertEquals("Erreur getY v1", 2, v1.getY(), EPSILON);
        assertEquals("Erreur getY v2", 2.12, v2.getY(), EPSILON);

    }

    /**
     * Tester la méthode setX.
     */
    @Test
    public void testSetX() {
        v1.setX(10);
        assertEquals("Erreur setX v1", 10, v1.getX(), EPSILON);
        v2.setX(-1);
        assertEquals("Erreur setX v2", -1, v2.getX(), EPSILON);
    }

    /**
     * Tester la méthode setY.
     */
    @Test
    public void testSetY() {
        v1.setY(10);
        assertEquals("Erreur setY v1", 10, v1.getY(), EPSILON);
        v2.setY(-1);
        assertEquals("Erreur setY v2", -1, v2.getY(), EPSILON);
    }

    /**
     * Tester la méthode setXY.
     */
    @Test
    public void testSetXY() {
        v1.setXY(10, 12);
        memesCoordonnees("Erreur setXY v1", new Vecteur2D(10, 12), v1);
        v2.setXY(-1, -2);
        memesCoordonnees("Erreur setXY v2", new Vecteur2D(-1, -2), v2);
    }

    /**
     * Tester la méthode norm2.
     */
    @Test
    public void testNorm2() {
        double norm2V1 = v1.getX() * v1.getX() + v1.getY() * v1.getY();
        assertEquals("Erreur norm2 v1", norm2V1, v1.norm2(), EPSILON);
        double norm2V2 = v2.getX() * v2.getX() + v2.getY() * v2.getY();
        assertEquals("Erreur norm2 v2", norm2V2, v2.norm2(), EPSILON);
    }

    /**
     * Tester la méthode add.
     */
    @Test
    public void testAdd() {
        v1.add(v2);
        memesCoordonnees("Erreur add v1", new Vecteur2D(7.4, 4.12), v1);
        v2.add(v1);
        memesCoordonnees("Erreur add v2", new Vecteur2D(9.8, 6.24), v2);
    }

    /**
     * Tester la méthode plus.
     */
    @Test
    public void testPlus() {
        Vecteur2D v1PlusV2 = new Vecteur2D(v1.getX() + v2.getX(), v1.getY() + v2.getY());
        memesCoordonnees("Erreur plus v1 + v2", v1PlusV2, v1.plus(v2));
        memesCoordonnees("Erreur plus v2", v1PlusV2, v2.plus(v1));
        Vecteur2D v1PlusV1 = new Vecteur2D(2 * v1.getX(), 2 * v1.getY());
        memesCoordonnees("Erreur plus 2*v1", v1PlusV1, v1.plus(v1));
    }

    /**
     * Tester la méthode minus.
     */
    @Test
    public void testMinus() {
        Vecteur2D v1MinusV2 = new Vecteur2D(v1.getX() - v2.getX(), v1.getY() - v2.getY());
        memesCoordonnees("Erreur minus v1 - v2", v1MinusV2, v1.minus(v2));
        Vecteur2D v2MinusV1 = new Vecteur2D(v2.getX() - v1.getX(), v2.getY() - v1.getY());
        memesCoordonnees("Erreur minus v2 - v1", v2MinusV1, v2.minus(v1));
        Vecteur2D v1MinusV1 = new Vecteur2D(0, 0);
        memesCoordonnees("Erreur minus v1 - v1", v1MinusV1, v1.minus(v1));
    }

    /**
     * Tester la méthode times.
     */
    @Test
    public void testTimes() {
        Vecteur2D v1Timesa = new Vecteur2D(a * v1.getX(), a * v1.getY());
        memesCoordonnees("Erreur times a * v1", v1Timesa, v1.times(a));
        Vecteur2D v1Timesb = new Vecteur2D(b * v1.getX(), b * v1.getY());
        memesCoordonnees("Erreur times b * v1", v1Timesb, v1.times(b));

        Vecteur2D v2Timesa = new Vecteur2D(a * v2.getX(), a * v2.getY());
        memesCoordonnees("Erreur times a * v2", v2Timesa, v2.times(a));

        Vecteur2D v2Timesb = new Vecteur2D(b * v2.getX(), b * v2.getY());
        memesCoordonnees("Erreur times b * v2", v2Timesb, v2.times(b));
    }

    /**
     * Tester la méthode divide.
     */
    @Test
    public void testDivide() {
        Vecteur2D v1Divide2 = new Vecteur2D(v1.getX() / a, v1.getY() / a);
        memesCoordonnees("Erreur divide v1 / 2.5", v1Divide2, v1.divide(a));
        Vecteur2D v2DivideMinus1 = new Vecteur2D(v2.getX() / b, v2.getY() / b);
        memesCoordonnees("Erreur divide v2 / -0.5", v2DivideMinus1, v2.divide(b));
    }

    /**
     * Tester la méthode dotProduct.
     */
    @Test
    public void testDotProduct() {
        double v1v2 = v1.getX() * v2.getX() + v1.getY() * v2.getY();
        assertEquals("Erreur dotProduct v1.v2", v1v2, v1.dotProduct(v2), EPSILON);
        assertEquals("Erreur dotProduct v2.v1", v1v2, v2.dotProduct(v1), EPSILON);

        double v1v1 = v1.getX() * v1.getX() + v1.getY() * v1.getY();
        assertEquals("Erreur dotProduct v1.v1", v1v1, v1.dotProduct(v1), EPSILON);
    }

    /**
     * Tester la méthode normalized.
     */
    @Test
    public void testNormalized() {
        memesCoordonnees("Erreur normalized v1", v1Normalized, v1.normalized());
        assertEquals("Erreur norm normalized v1", 1, v1Normalized.norm2(), EPSILON);

        memesCoordonnees("Erreur normalized v2", v2Normalized, v2.normalized());
        assertEquals("Erreur norm normalized v2", 1, v2Normalized.norm2(), EPSILON);
    }

    /**
     * Tester la méthode normalize.
     */
    @Test
    public void testNormalize() {
        v1.normalize();
        memesCoordonnees("Erreur normalize v1", v1Normalized, v1);
        assertEquals("Erreur norm normalize v1", 1, v1.norm2(), EPSILON);

        v2.normalize();
        memesCoordonnees("Erreur normalize v2", v2Normalized, v2);
        assertEquals("Erreur norm normalize v2", 1, v2.norm2(), EPSILON);
    }

    /**
     * Tester la méthode copy.
     */
    @Test
    public void testCopy() {
        Vecteur2D v1Copy = v1.copy();
        memesCoordonnees("Erreur copy v1", v1, v1Copy);
        Vecteur2D v2Copy = v2.copy();
        memesCoordonnees("Erreur copy v2", v2, v2Copy);
    }
}
