package simulation2D.rendering;

import java.util.List;
import java.awt.Graphics2D;

import simulation2D.objects.Particule;
import simulation2D.simulation.SimulationState;


/**
 * La classe <code>Affichage</code> est responsable de l'affichage des particules.
 *
 * @author Vianney Hervy
 */
public class Affichage {

    /**
     * L'état de la simulation à afficher.
     */
    private SimulationState simulationState;

    /**
     * Construire un nouvel <code>Affichage</code> avec l'état de la simulation.
     * @param simulationState l'état de la simulation
     */
    public Affichage(SimulationState simulationState) {
        this.simulationState = simulationState;
    }

    /**
     * Afficher les particules de la simulation.
     * @param g le contexte graphique
     */
    public void render(Graphics2D g) {
        List<Particule> particules = simulationState.getParticles();
        for (Particule particule : particules) {
            particule.draw(g);
        }
    }
}
