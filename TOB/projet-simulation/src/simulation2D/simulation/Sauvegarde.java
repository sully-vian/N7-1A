package simulation2D.simulation;

import simulation2D.objects.Famille;
import simulation2D.objects.Particule;

import java.io.FileWriter;
import java.util.List;

/**
 * La classe <code>Sauvegarde</code> permet de sauvegarder l'état initial de la
 * simulation. Son développement a été abandonné, son code est maintenant dans
 * la classe <code>Configuration</code>
 *
 * @author Thomas SABATIER
 */
@Deprecated
public class Sauvegarde {

    /**
     * Le nom de la sauvegarde.
     */
    private String nom;

    /**
     * L'état initial de la simulation à sauvegarder.
     */
    private SimulationState etatInitial;

    /**
     * Créer une sauvegarde de l'état initial de la simulation.
     *
     * @param nomSauvegarde   Le nom de la sauvegarde.
     * @param simulationState L'état initial de la simulation à sauvegarder.
     */
    public Sauvegarde(String nomSauvegarde, SimulationState simulationState) {
        this.etatInitial = simulationState;
        this.nom = nomSauvegarde;
    }

    /**
     * Créer une sauvegarde de l'état initial de la simulation.
     *
     * @throws Exception Si une erreur survient lors de la sauvegarde.
     */
    public Sauvegarde() throws Exception {
        // On créer le fichier de sauvegarde
        FileWriter fichierSave = new FileWriter(this.nom);

        // On récupère les données à sauvegarder
        List<Particule> listeParticule = this.etatInitial.getParticles();
        // DicoRelations2D dicoRelations = this.etatInitial.getRelations();
        List<Famille> listeFamille = this.etatInitial.getFamilles();

        // Pour l'instant on ne sauvegarde que quelques paramètres
        // On trouvera dans le fichier le nombre de particule par famille puis le nombre
        // de famille
        fichierSave.write(Integer.toString(listeParticule.size()));
        fichierSave.write(Integer.toString(listeFamille.size()));

        fichierSave.close();
    }
}
