package simulation2D.statistiques;

import java.util.ArrayList;
import java.util.List;

import simulation2D.Launch;
import simulation2D.objects.Particule;
import simulation2D.simulation.SimulationState;

/**
 * Gère les statistiques d'énergie cinétique.
 *
 * @author Baptiste Gomez
 */
public class EnergieCinetique implements Statistique {

    /**
     * Les données à afficher.
     */
    private List<Double> data;

    private double updateIteration;

    private double energie;

    /**
     * Créer une nouvelle statistique d'énergie cinétique.
     */
    public EnergieCinetique() {
        this.data = new ArrayList<>();
        this.updateIteration = 0;
        this.energie = 0;
    }

    /**
     * Mettre à jour les statistiques d'énergie cinétique.
     *
     * @param state l'état de la simulation
     */
    @Override
    public void updateStat(SimulationState state) {
        List<Particule> particules = state.getParticles();
        double energieCinetique = 0;
        int nbParticules = particules.size();
        updateIteration++;
        for (Particule particule : particules) {
            double vitesse = particule.getVelocity().norm2();
            double masse = particule.getMass();
            energieCinetique += 0.5 * masse * vitesse * vitesse;
        }
        energieCinetique = energieCinetique / nbParticules;
        if (updateIteration < Launch.FPS) {
            energie += energieCinetique;
        } else {
            this.data.add(energie);
            updateIteration = 0;
            energie = 0;
        }
    }

    /**
     * Obtenir les données de statistique d'énergie cinétique.
     */
    @Override
    public List<Double> getData() {
        return this.data;
    }

    /**
     * Afficher les statistiques d'énergie cinétique.
     */
    @Override
    public void afficher() {
        System.out.print("Energie cinétique : ");
        for (double nb : this.data) {
            System.out.print(nb);
            System.out.print(" ; ");
        }
        System.out.print("\n");
    }
}
