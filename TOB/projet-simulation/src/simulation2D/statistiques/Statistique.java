package simulation2D.statistiques;

import java.util.List;

import simulation2D.simulation.SimulationState;

/**
 * Défini ce qu'est une statistique.
 *
 * @author Baptiste Gomez
 */
public interface Statistique {

    /**
     * Les données relative à la statistique en question.
     *
     * @return les données relative à la statistique en question
     */
    List<Double> getData();

    /**
     * Mettre à jour les statistiques à l'aide de l'état actuel de la simulation.
     *
     * @param state l'état actuel de la simulation
     */
    void updateStat(SimulationState state);

    /**
     * afficher la statistiqueconcernée.
     */
    void afficher();
}
