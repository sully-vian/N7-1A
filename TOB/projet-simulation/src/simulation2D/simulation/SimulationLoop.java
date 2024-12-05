package simulation2D.simulation;

import javax.swing.SwingUtilities;
import javax.swing.JPanel;
//import java.time.LocalDate;

import simulation2D.Launch;
import simulation2D.physics.Force;
import simulation2D.physics.MoteurPhysique;
import simulation2D.rendering.AffichageStat;
import simulation2D.physics.Collision;
import simulation2D.statistiques.CollisionStat;
import simulation2D.statistiques.EnergieCinetique;
import simulation2D.statistiques.DensityStat;

// Une classe qui gère la boucle de simulation, y compris le temps, les mises à
// jour de l'état de la simulation et le rendu graphique.

/**
 * La classe <code>SimulationLoop</code> gère la boucle de simulation, y compris
 * le temps, les mises à jour de l'état de la simulation et le rendu graphique.
 *
 * @author Vianney Hervy
 */
public class SimulationLoop implements Runnable {

    /**
     * L'état de la boucle de simulation (true si la boucle est en cours, false
     * sinon).
     */
    private boolean running = false;

    /**
     * L'état de la simulation.
     */
    private SimulationState simulationState;

    /**
     * Le panneau de dessin de la simulation.
     */
    private JPanel panel;

    /**
     * Construire une boucle de simulation étant donné un état de simulation et un
     * panneau de dessin.
     *
     * @param simulationState l'état de la simulation
     * @param panel           le panneau de dessin
     */
    public SimulationLoop(SimulationState simulationState, JPanel panel) {
        this.simulationState = simulationState;
        this.panel = panel;
    }

    /**
     * Démarrer la boucle de simulation.
     */
    public void start() {
        running = true;
        new Thread(this).start();
    }

    /**
     * Arrêter la boucle de simulation.
     */
    public void stop() {
        running = false;
    }

    /**
     * Exécuter la boucle de simulation.
     */
    @Override
    public void run() {
        CollisionStat statistiqueCollision = new CollisionStat();
        EnergieCinetique statistiqueEnergieCinetique = new EnergieCinetique();
        DensityStat statistiqueDensite = new DensityStat(5);
        AffichageStat windowCin = new AffichageStat(statistiqueEnergieCinetique, -68);
        AffichageStat windowCol = new AffichageStat(statistiqueCollision, 424);
        long milliseconds = System.currentTimeMillis();

        while (running) {
            long currentMilliseconds = System.currentTimeMillis();
            // mise à jour de l'état de la simulation
            Force.calculateForces(simulationState.getParticles(), simulationState.getRelations());
            MoteurPhysique.applyForces(simulationState.getParticles());
            Collision.checkWallCollisions(simulationState.getParticles());
            int collisions = Collision.checkParticlesCollisions(simulationState.getParticles());
            this.simulationState.setNbCollisions(collisions);
            MoteurPhysique.updatePositions(simulationState.getParticles());

            statistiqueCollision.updateStat(simulationState);
            statistiqueEnergieCinetique.updateStat(simulationState);
            statistiqueDensite.updateStat(simulationState);
            if (currentMilliseconds - milliseconds > 1000) {
                windowCin.update(statistiqueEnergieCinetique);
                windowCol.update(statistiqueCollision);
                // statistiqueDensite.afficher();
                milliseconds = currentMilliseconds;
            }
            // demander une mise à jour du rendu
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    panel.repaint();
                }
            });

            // Pause pour limiter la vitesse de la boucle
            try {
                Thread.sleep(1000 / Launch.FPS);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

}
