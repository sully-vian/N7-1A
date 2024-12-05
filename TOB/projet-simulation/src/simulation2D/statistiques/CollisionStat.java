package simulation2D.statistiques;

import java.util.ArrayList;
import java.util.List;
import simulation2D.Launch;
import simulation2D.simulation.SimulationState;

/**
 * Gère les statistiques de collisions.
 *
 * @author Baptiste Gomez
 */
public class CollisionStat implements Statistique {

    /**
     * Les statistiques de collision.
     */
    private List<Double> data;
    private double nbcollision;
    private int updateIteration;
    /**
     * Créer un nouvel objet gérant les statistiques de collision.
     */
    public CollisionStat() {
        this.data = new ArrayList<>();
        this.updateIteration = 0;
        this.nbcollision = 0;
    }

    /**
     * Obtenir les données sur les colisions.
     */
    @Override
    public List<Double> getData() {
        return this.data;
    }

    /**
     * Mettre à jour les données sur les collisions à l'aide de l'état actuel de la simulation.
     *
     * @param state l'état actuel de la simulation
     */
    @Override
    public void updateStat(SimulationState state) {
        double nbParticules = state.getParticles().size();
        updateIteration++;
        if (updateIteration < Launch.FPS) { 
            this.nbcollision += state.getNbCollisions(); 
        } else {
            this.data.add(nbcollision / nbParticules);
            this.nbcollision = 0;
            this.updateIteration = 0;
        }  
    } 
    /**
     * Afficher les statistiques concernant les collisions.
     */
    public void afficher() {
        System.out.print("Nombre de collisions : ");
        for (double nb : this.data) {
            System.out.print(nb);
            System.out.print(" ; ");
        }
        System.out.print("\n");
    }
}
