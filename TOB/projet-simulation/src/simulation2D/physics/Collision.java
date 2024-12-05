package simulation2D.physics;

import java.util.List;

import simulation2D.Launch;
import simulation2D.objects.Particule;

/**
 * La classe <code>Collision</code> est responsable de la gestion des
 * collisions avec les murs et entre les particules.
 *
 * @author Vianney Hervy
 */
public class Collision {

    /**
     * Le facteur de rebondissement. C'est la proportion d'énergie conservée lors
     * d'une collision. Si BOUNCE = 1, la collision est parfaitement élastique.
     */
    private static final double BOUNCE = 1;

    /**
     * Constructeur privé pour empêcher l'instanciation de la classe.
     */
    private Collision() {
    }

    /**
     * Vérifier les collisions avec les murs pour toutes les particules.
     *
     * @param particules la liste des particules
     */
    public static void checkWallCollisions(List<Particule> particules) {
        for (Particule particule : particules) {
            checkWallCollision(particule);
        }
    }

    /**
     * Vérifier les collisions avec les murs pour une particule.
     *
     * @param particule la particule dont on veut vérifier la collision avec les
     *                  murs
     */
    public static void checkWallCollision(Particule particule) {

        if (particule.getLeft() <= 0) {
            handleLeftWallCollision(particule);
        }

        if (particule.getRight() >= Launch.WIDTH) {
            handleRightWallCollision(particule);
        }

        if (particule.getTop() <= 0) {
            handleCeilingCollision(particule);
        }

        if (particule.getBottom() >= Launch.HEIGHT) {
            handleFloorCollision(particule);
        }
    }

    /**
     * Gérer la collision avec le mur de gauche.
     *
     * @param particule la particule qui a collisionné avec le mur de gauche
     */
    public static void handleLeftWallCollision(Particule particule) {
        particule.setLeft(0);
        Vecteur2D velocity = particule.getVelocity();
        particule.setVelocity(velocity.getX() * -BOUNCE, velocity.getY());
    }

    /**
     * Gérer la collision avec le mur de droite.
     *
     * @param particule la particule qui a collisionné avec le mur de droite
     */
    public static void handleRightWallCollision(Particule particule) {
        particule.setRight(Launch.WIDTH);
        Vecteur2D velocity = particule.getVelocity();
        particule.setVelocity(velocity.getX() * -BOUNCE, velocity.getY());
    }

    /**
     * Gérer la collision avec le plafond (mur supérieur).
     *
     * @param particule la particule qui a collisionné avec le plafond
     */
    public static void handleCeilingCollision(Particule particule) {
        particule.setTop(0);
        Vecteur2D velocity = particule.getVelocity();
        particule.setVelocity(velocity.getX(), velocity.getY() * -BOUNCE);
    }

    /**
     * Gérer la collision avec le sol (mur inférieur).
     *
     * @param particule la particule qui a collisionné avec le sol
     */
    public static void handleFloorCollision(Particule particule) {
        particule.setBottom(Launch.HEIGHT);
        Vecteur2D velocity = particule.getVelocity();
        particule.setVelocity(velocity.getX(), velocity.getY() * -BOUNCE);
    }

    /**
     * Vérifier les collisions entre les particules.
     *
     * @param particules la liste des particules
     * @return le nombre de collisions ayant eu lieu
     */
    public static int checkParticlesCollisions(List<Particule> particules) {
        int collisions = 0;
        for (int i = 0; i < particules.size(); i++) {
            Particule particuleA = particules.get(i);
            for (int j = i + 1; j < particules.size(); j++) {
                Particule particuleB = particules.get(j);
                double distance = particuleA.isOverlapping(particuleB);

                if (distance != -1) { /* collision */
                    collisions += 1;
                    handleCollision(particuleA, particuleB, distance);
                }
            }
        }
        return collisions;
    }

    /**
     * Gérer la collision entre deux particules.
     *
     * @param particuleA la première particule
     * @param particuleB la seconde particule
     * @param distance   la distance entre les centres des deux particules
     */
    public static void handleCollision(Particule particuleA, Particule particuleB, double distance) {
        // Vecteur du centre de A au centre de B
        Vecteur2D normal = particuleA.getPosition().minus(particuleB.getPosition());
        normal.normalize();

        separateParticles(particuleA, particuleB, normal, distance);

        // collisionner les deux particules
        // collide(particuleA, particuleB, normal);
    }

    /**
     * Collisionner les deux particules. Calculer et appliquer les nouvelles
     * vitesses après la collisions
     *
     * @param particuleA la première particule
     * @param particuleB la seconde particule
     * @param normal     le vecteur normalisé du centre de A au centre de B
     */
    public static void collide(Particule particuleA, Particule particuleB, Vecteur2D normal) {
        // vitesse relative
        Vecteur2D deltaV = particuleA.getVelocity().minus(particuleB.getVelocity());

        // vitesse relative selon la normale
        double normalVelocity = deltaV.dotProduct(normal);
        if (normalVelocity > 0) {
            // les particules se séparent déjà (elles sont passées l'un à travers l'autre)
            return;
        }

        // obtenir les masses des particules
        double mA = particuleA.getMass();
        double mB = particuleB.getMass();
        double mTot = mA + mB;

        // mettre à l'échelle la vitesse normale par la masse : plus la masse est
        // grande, moins la vitesse est affectée
        double scaledNormVelA = (2 * mB / mTot) * normalVelocity;
        double scaledNormVelB = (2 * mA / mTot) * normalVelocity;

        // calculer les nouvelles vitesses
        Vecteur2D vANew = particuleA.getVelocity().minus(normal.times(scaledNormVelA));
        Vecteur2D vBNew = particuleB.getVelocity().plus(normal.times(scaledNormVelB));

        // changer la vitesse des particules en appliquant le facteur de rebond
        particuleA.setVelocity(vANew.times(BOUNCE));
        particuleB.setVelocity(vBNew.times(BOUNCE));
    }

    /**
     * Séparer des particules se chevauchant.
     *
     * @param particuleA la première particule
     * @param particuleB la seconde particule
     * @param normal     le vecteur normalisé du centre de A au centre de B
     * @param distance   la distance entre les centres des deux particules
     */
    public static void separateParticles(Particule particuleA, Particule particuleB, Vecteur2D normal,
            double distance) {

        double overlap = particuleA.getRadius() + particuleB.getRadius() - distance;

        particuleA.translate(normal.times(overlap / 2));
        particuleB.translate(normal.times(-overlap / 2));
    }

}
