package simulation2D.physics;

import java.util.List;

import simulation2D.objects.DicoRelations2D;
import simulation2D.objects.DicoRelations;
import simulation2D.objects.Particule;

/**
 * La classe <code>Force</code> est responsable du calcul des forces.
 *
 * @author Vianney Hervy
 */
public class Force {

    /**
     * La constante EPSILON de distance minimale.
     */
    private static final double EPSILON = 1;

    /**
     * Constructeur privé pour empêcher l'instanciation de la classe.
     */
    private Force() {
    }

    /**
     * Calculer les forces pour tous les particules.
     *
     * @param particules la liste des particules
     * @param relations  le dictionnaire des relations entre les familles
     */
    public static void calculateForces(List<Particule> particules, DicoRelations2D relations) {
        for (Particule particuleInfluencee : particules) {
            DicoRelations relationsParticule = relations.getRelations(particuleInfluencee.getFamille());
            for (Particule particuleInfluente : particules) {
                calculateForce(particuleInfluencee, particuleInfluente, relationsParticule);
            }
        }
    }

    /**
     * Calculer la force d'une particule sur une autre.
     *
     * @param particuleInfluencee la particule influencée
     * @param particuleInfluente  la particule influente
     * @param relations           le dictionnaire des relations sur la famille de la
     *                            particule influencée
     */
    public static void calculateForce(Particule particuleInfluencee, Particule particuleInfluente,
            DicoRelations relations) {
        double dx = particuleInfluente.getX() - particuleInfluencee.getX();
        double dy = particuleInfluente.getY() - particuleInfluencee.getY();

        Vecteur2D force = new Vecteur2D(dx, dy);

        double dist2 = force.norm2();
        force = force.times(relations.getIntensite(particuleInfluente.getFamille()));
        if (dist2 > EPSILON) { /* pas de force sur soi-même ou les particules trop proches */
            force = force.divide(dist2);
        }
        particuleInfluencee.addForce(force);
    }

}
