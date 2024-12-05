package simulation2D.simulation;

import java.util.List;


import simulation2D.objects.Particule;
import simulation2D.objects.Famille;
import simulation2D.objects.DicoRelations2D;

/**
 * La classe <code>SimulationState</code> gère l'état de la simulation, y
 * compris toutes les particules actuellement dans la simulation.
 *
 * @author Vianney Hervy
 */
public class SimulationState {

    /**
     * La configuration de la simulation.
     */
    private Configuration configuration;

    /**
     * La liste des particules actuellement dans la simulation.
     */
    private List<Particule> particules;

    /**
     * Le dictionnaire des relations entre les familles de particules.
     */
    private DicoRelations2D relations;

    /**
     * La liste des familles de particules.
     */
    private List<Famille> familles;

    /**
     * Le nombre de collisions.
     */
    private int nbCollisions;

    /**
     * Construire un état de simulation avec les particules, les relations et les
     * familles donnés.
     *
     * @param particules les particules actuellement dans la simulation
     * @param relations  les relations entre les familles
     * @param familles   les familles de particules
     */
    public SimulationState(List<Particule> particules, Configuration configuration) {
        this.particules = particules;
        this.configuration = configuration;
        this.relations = this.configuration.getRelations();
        this.familles = this.configuration.getFamilles();
        this.nbCollisions = 0;
    }

    /**
     * Obtenir la liste des particules actuellement dans la simulation.
     *
     * @return la liste des particules actuellement dans la simulation
     */
    public List<Particule> getParticles() {
        return particules;
    }

    /**
     * Obtenir le dictionnaire des relations entre les familles de particules.
     *
     * @return le dictionnaire des relations entre les familles de particules
     */
    public DicoRelations2D getRelations() {
        return relations;
    }

    /**
     * Obtenir la liste des familles de particules.
     *
     * @return la liste des familles de particules
     */
    public List<Famille> getFamilles() {
        return familles;
    }

    /**
     * Obtenir le nombre de collisions.
     *
     * @return le nombre de collisions
     */
    public int getNbCollisions() {
        return this.nbCollisions;
    }

    /**
     * Modifier le nombre de collisions.
     *
     * @param nb le nouveau nombre de collisions
     */
    public void setNbCollisions(int nb) {
        this.nbCollisions = nb;
    }
}
