package simulation2D.tests;

import simulation2D.physics.Vecteur2D;
import simulation2D.objects.Particule;
import simulation2D.objects.Famille;

import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.Test;

import java.awt.Color;

/**
 * Classe de Tester la classe Particule.
 *
 * @see simulation2D.objects.Particule
 *
 * @author Yassine Bougacha & Vianney Hervy
 */
public class ParticuleTest {

    /**
     * La précision pour les comparaisons réelles.
     */
    private static final double EPSILON = 0.001;

    /**
     * Les particules testées.
     */
    private Particule p1, p2;

    /**
     * Les vecteurs positions testés.
     */
    private Vecteur2D pos1, pos2;

    /**
     * Les vecteurs vitesses testés.
     */
    private Vecteur2D vel1, vel2;

    /**
     * Les vecteurs forces testés.
     */
    private Vecteur2D force1, force2;

    /**
     * La famille des particules testées.
     */
    private final Famille famille = new Famille("F", Color.BLUE, 5, 5, 1000);

    /**
     * Vérifier si deux vecteurs ont les mêmes coordonnées.
     *
     * @param message le message d'erreur
     * @param vect1   le premier vecteur
     * @param vect2   le second vecteur
     */
    static void memesCoordonnees(String message, Vecteur2D vect1, Vecteur2D vect2) {
        assertEquals(message + "(x)", vect1.getX(), vect2.getX(), EPSILON);
        assertEquals(message + "(y)", vect1.getY(), vect2.getY(), EPSILON);
    }

    /**
     * Initialiser des états avant chaque test.
     */
    @Before
    public void setUp() {
        pos1 = new Vecteur2D(1, 2);
        pos2 = new Vecteur2D(3, 4);

        vel1 = new Vecteur2D(10, 20);
        vel2 = new Vecteur2D(30, 40);

        force1 = new Vecteur2D(100, 200);
        force2 = new Vecteur2D(300, 400);

        p1 = new Particule(famille, pos1, vel1);
        p2 = new Particule(famille, pos2, vel2);
    }

    /**
     * Tester la méthode getFamille.
     */
    @Test
    public void testGetFamille() {
        // TODO
    }

    /**
     * Tester la méthode getX.
     */
    @Test
    public void testGetX() {
        assertEquals("Erreur getX p1", pos1.getX(), p1.getX(), EPSILON);
        assertEquals("Erreur getX p2", pos2.getX(), p2.getX(), EPSILON);
    }

    /**
     * Tester la méthode getY.
     */
    @Test
    public void testGetY() {
        assertEquals("Erreur getY p1", pos1.getY(), p1.getY(), EPSILON);
        assertEquals("Erreur getY p2", pos2.getY(), p2.getY(), EPSILON);
    }

    /**
     * Tester la méthode getPosition.
     */
    @Test
    public void testGetPosition() {
        memesCoordonnees("Erreur getPosition p1", pos1, p1.getPosition());
        memesCoordonnees("Erreur getPosition p2", pos2, p2.getPosition());
    }

    /**
     * Tester la méthode setPosition(Vecteur2D).
     */
    @Test
    public void testSetPosition1() {
        p1.setPosition(pos2);
        memesCoordonnees("Erreur setPosition1 p1", pos2, p1.getPosition());
        p2.setPosition(pos1);
        memesCoordonnees("Erreur setPosition1 p2", pos1, p2.getPosition());
    }

    /**
     * Tester la méthode setPosition(double, double).
     */
    @Test
    public void testSetPosition2() {
        p1.setPosition(pos2.getX(), pos2.getY());
        memesCoordonnees("Erreur setPosition1 p1", pos2, p1.getPosition());
        p2.setPosition(pos1.getX(), pos1.getY());
        memesCoordonnees("Erreur setPosition1 p2", pos1, p2.getPosition());
    }

    /**
     * Tester la méthode translate.
     */
    @Test
    public void testTranslate() {
        p1.translate(pos2);
        memesCoordonnees("Erreur translate p1", pos1.plus(pos2), p1.getPosition());
        p2.translate(pos1);
        memesCoordonnees("Erreur translate p2", pos2.plus(pos1), p2.getPosition());
    }

    /**
     * Tester la méthode getMass.
     */
    @Test
    public void testGetMass() {
        assertEquals("Erreur getMass p1", 5, p1.getMass(), EPSILON);
        assertEquals("Erreur getMass p2", 5, p2.getMass(), EPSILON);
    }

    /**
     * Tester la méthode getRadius.
     */
    @Test
    public void testGetRadius() {
        assertEquals("Erreur getMass p1", 5, p1.getRadius(), EPSILON);
        assertEquals("Erreur getMass p2", 5, p2.getRadius(), EPSILON);
    }

    /**
     * Tester la méthode getLeft.
     */
    @Test
    public void testGetLeft() {
        assertEquals("Erreur getLeft p1", p1.getX() - p1.getRadius(), p1.getLeft(), EPSILON);
        assertEquals("Erreur getLeft p2", p2.getX() - p2.getRadius(), p2.getLeft(), EPSILON);
    }

    /**
     * Tester la méthode getRight.
     */
    @Test
    public void testGetRight() {
        assertEquals("Erreur getRight p1", p1.getX() + p1.getRadius(), p1.getRight(), EPSILON);
        assertEquals("Erreur getRight p2", p2.getX() + p2.getRadius(), p2.getRight(), EPSILON);
    }

    /**
     * Tester la méthode getTop.
     */
    @Test
    public void testGetTop() {
        assertEquals("Erreur getTop p1", p1.getY() - p1.getRadius(), p1.getTop(), EPSILON);
        assertEquals("Erreur getTop p2", p2.getY() - p2.getRadius(), p2.getTop(), EPSILON);
    }

    /**
     * Tester la méthode getBottom.
     */
    @Test
    public void testGetBottom() {
        assertEquals("Erreur getBottom p1", p1.getY() + p1.getRadius(), p1.getBottom(), EPSILON);
        assertEquals("Erreur getBottom p2", p2.getY() + p2.getRadius(), p2.getBottom(), EPSILON);
    }

    /**
     * Tester la méthode setLeft.
     */
    @Test
    public void testSetLeft() {
        p1.setLeft(1);
        assertEquals("Erreur setLeft p1", 1, p1.getX() - p1.getRadius(), EPSILON);
        p2.setLeft(2);
        assertEquals("Erreur setLeft p2", 2, p2.getX() - p2.getRadius(), EPSILON);
    }

    /**
     * Tester la méthode setRight.
     */
    @Test
    public void testSetRight() {
        p1.setRight(1);
        assertEquals("Erreur setRight p1", 1, p1.getX() + p1.getRadius(), EPSILON);
        p2.setRight(2);
        assertEquals("Erreur setRight p2", 2, p2.getX() + p2.getRadius(), EPSILON);
    }

    /**
     * Tester la méthode setTop.
     */
    @Test
    public void testSetTop() {
        p1.setTop(1);
        assertEquals("Erreur setTop p1", 1, p1.getY() - p1.getRadius(), EPSILON);
        p2.setTop(2);
        assertEquals("Erreur setTop p2", 2, p2.getY() - p2.getRadius(), EPSILON);
    }

    /**
     * Tester la méthode setBottom.
     */
    @Test
    public void testSetBottom() {
        p1.setBottom(1);
        assertEquals("Erreur setBottom p1", 1, p1.getY() + p1.getRadius(), EPSILON);
        p2.setBottom(2);
        assertEquals("Erreur setBottom p2", 2, p2.getY() + p2.getRadius(), EPSILON);
    }

    /**
     * Tester la méthode getVelocity.
     */
    @Test
    public void getVelocity() {
        memesCoordonnees("Erreur getVelocity p1", vel1, p1.getVelocity());
        memesCoordonnees("Erreur getVelocity p2", vel2, p2.getVelocity());
    }

    /**
     * Tester la méthode setVelocity(Vecteur2D).
     */
    @Test
    public void setVelocity1() {
        p1.setVelocity(vel2);
        memesCoordonnees("Erreur setVelocity1 p1", vel2, p1.getVelocity());
        p2.setVelocity(vel1);
        memesCoordonnees("Erreur setVelocity1 p2", vel1, p2.getVelocity());
    }

    /**
     * Tester la méthode setVelocity(double, double).
     */
    @Test
    public void setVelocity2() {
        p1.setVelocity(vel2.getX(), vel2.getY());
        memesCoordonnees("Erreur setVelocity2 p1", vel2, p1.getVelocity());
        p2.setVelocity(vel1.getX(), vel1.getY());
        memesCoordonnees("Erreur setVelocity2 p2", vel1, p2.getVelocity());
    }

    /**
     * Tester la méthode addVelocity.
     *
     */
    @Test
    public void addVelocity() {
        p1.addVelocity(vel2);
        memesCoordonnees("Erreur addVelocity p1", vel1.plus(vel2), p1.getVelocity());
        p2.addVelocity(vel1);
        memesCoordonnees("Erreur addVelocity p2", vel2.plus(vel1), p2.getVelocity());
    }

    /**
     * Tester la méthode getForce.
     */
    @Test
    public void getForce() {
        memesCoordonnees("Erreur getForce p1", new Vecteur2D(0, 0), p1.getForce());
        p1.setForce(force1);
        memesCoordonnees("Erreur getForce p1", force1, p1.getForce());

        memesCoordonnees("Erreur getForce p2", new Vecteur2D(0, 0), p2.getForce());
        p2.setForce(force2);
        memesCoordonnees("Erreur getForce p2", force2, p2.getForce());
    }

    /**
     * Tester la méthode setForce.
     */
    @Test
    public void setForce() {
        p1.setForce(force2);
        memesCoordonnees("Erreur setForce p1", force2, p1.getForce());
        p2.setForce(force1);
        memesCoordonnees("Erreur setForce p2", force1, p2.getForce());
    }

    /**
     * Tester la méthode addForce.
     */
    @Test
    public void addForce() {
        p1.setForce(vel1);
        p1.addForce(vel2);
        memesCoordonnees("Erreur addForce p1", vel1.plus(vel2), p1.getForce());
        p2.setForce(vel2);
        p2.addForce(vel1);
        memesCoordonnees("Erreur addForce p2", vel2.plus(vel1), p2.getForce());
    }

    /**
     * Tester la méthode resetForce.
     */
    @Test
    public void resetForce() {
        p1.setForce(vel1);
        p1.resetForce();
        memesCoordonnees("Erreur resetForce p1", new Vecteur2D(0, 0), p1.getForce());
        p2.setForce(vel2);
        p2.resetForce();
        memesCoordonnees("Erreur resetForce p2", new Vecteur2D(0, 0), p2.getForce());
    }

    /**
     * Tester la méthode isOverlapping.
     */
    @Test
    public void testIsOverlapping() {
        // TODO
    }

}
