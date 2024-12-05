package simulation2D.physics;

import java.util.List;

import simulation2D.Launch;
import simulation2D.objects.Particule;

/**
 * La classe <code>MoteurPhysique</code> est responsable de la gestion de la
 * physique. Elle ne centralise pas tout.
 *
 * @author Vianney Hervy
 */
public class MoteurPhysique {

    /**
     * Le pas de temps en secondes.
     */
    private static final double DELTA_T = 1.0 / Launch.FPS;

    /**
     * Constructeur privé pour empêcher l'instanciation de la classe.
     */
    private MoteurPhysique() {
    }

    /**
     * Appliquer les forces aux particules. C'est à dire calculer leur nouveau
     * vecteur
     * vitesse.
     *
     * @param particules la liste des particules
     */
    public static void applyForces(List<Particule> particules) {
        for (Particule particule : particules) {
            applyForces(particule);
        }
    }

    /**
     * Appliquer les forces à une particule.
     *
     * @param particule la particule
     */
    public static void applyForces(Particule particule) {
        Vecteur2D acceleration = particule.getForce().divide(particule.getMass());
        Vecteur2D addedVelocity = acceleration.times(DELTA_T);
        particule.addVelocity(addedVelocity);
        particule.resetForce();
    }

    /**
     * Mettre à jour les positions des particules.
     *
     * @param particules la liste des particules
     */
    public static void updatePositions(List<Particule> particules) {
        for (Particule particule : particules) {
            updatePosition(particule);
        }
    }

    /**
     * Mettre à jour la position de la particule.
     *
     * @param particule la particule dont la position doit être mise à jour
     */
    public static void updatePosition(Particule particule) {
        Vecteur2D displacement = particule.getVelocity().times(DELTA_T);
        particule.translate(displacement);
    }

}
