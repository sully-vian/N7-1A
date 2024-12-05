package simulation2D.simulation;

import simulation2D.objects.DicoRelations2D;
import simulation2D.objects.Famille;

import java.util.List;
import java.util.ArrayList;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * La classe <code>Configuration</code> gère les paramètres d'une simulation.
 * Les valeurs de ses attributs peut être fixées par l'utilisateur via le
 * <code>MenuGeneral</code> ou en chargeant une sauvegarde avec
 * <code>Sauvegarde</code>.
 *
 * @author Thomas SBATIER & Vianney Hervy
 */
public class Configuration {

    /**
     * Les familles de particules.
     */
    private List<Famille> familles;

    /**
     * Le dictionnaire des relations entre les familles de particules.
     */
    private DicoRelations2D relations;

    /**
     * Créer une configuration de simulation avec les familles et les relations
     * données.
     *
     * @param familles  les familles de particules
     * @param relations les relations entre les familles de particules
     */
    public Configuration(List<Famille> familles, DicoRelations2D relations) {
        this.familles = familles;
        this.relations = relations;
    }

    /**
     * Créer une configuration de simulation par défaut. Cette configuration
     * contient 3 familles rouges de 10 particules de rayon 5 et de masse 1. La
     * vitesse initiale des particules est nulle. Les relations créées sont
     * également toutes d'intensité nulle.
     *
     */
    public Configuration() {
        this(new ArrayList<Famille>(), new DicoRelations2D());

        // ArrayList<Famille> familles = new ArrayList<>();
        this.ajouterFamille(new Famille("Famille 1"));
        this.ajouterFamille(new Famille("Famille 2"));
        this.ajouterFamille(new Famille("Famille 3"));

    }

    /**
     * Obtenir les familles de particules.
     *
     * @return les familles de particules
     */
    public List<Famille> getFamilles() {
        return familles;
    }

    /**
     * Obtenir le nombre de familles de particules.
     *
     * @return le nombre de familles de particules
     */
    public int getNbFamilles() {
        return familles.size();
    }

    /**
     * Obtenir le nombre total de particules de la simulation.
     *
     * @return le nombre total de particules de la simulation
     */
    public int getNbTotalParticules() {
        int nbTotalParticules = 0;
        for (Famille famille : familles) {
            nbTotalParticules += famille.getNbParticules();
        }
        return nbTotalParticules;
    }

    /**
     * Obtenir les relations entre les familles de particules.
     *
     * @return les relations entre les familles de particules
     */
    public DicoRelations2D getRelations() {
        return relations;
    }

    public void ajouterFamille(Famille famille) {
        this.familles.add(famille);
        this.relations.creerRelations(famille);
    }

    /**
     * Supprimer une famille de particules de la configuration. Ses relations
     * avec les autres familles sont également supprimées.
     *
     * @param famille la famille à supprimer
     */
    public void supprimerFamille(Famille famille) {
        this.familles.remove(famille);
        this.relations.supprimerRelations(famille);
    }

    /**
     * Créer une configuration de simulation à partir de sa représentation JSON.
     *
     * @param configurationJSON la représentation JSON de la configuration
     * @return la configuration de simulation
     */
    public static Configuration fromJSON(JSONObject configurationJSON) {
        JSONObject relationsJSON = configurationJSON.getJSONObject("relations");
        DicoRelations2D relations = DicoRelations2D.fromJSON(relationsJSON);

        JSONObject famillesJSON = configurationJSON.getJSONObject("familles");
        List<Famille> familles = new ArrayList<>();
        for (String nomFamille : famillesJSON.keySet()) {
            JSONObject familleJSON = famillesJSON.getJSONObject(nomFamille);
            Famille famille = Famille.fromJSON(familleJSON);
            familles.add(famille);
        }

        return new Configuration(familles, relations);
    }

    /**
     * Construire la représentation JSON de la configuration.
     *
     * @return la représentation JSON de la configuration
     */
    public JSONObject toJSON() {
        JSONObject configurationJSON = new JSONObject();

        JSONObject famillesJSON = new JSONObject();
        for (Famille famille : this.familles) {
            famillesJSON.put(famille.getNom(), famille.toJSON());
        }

        JSONObject relationsJSON = this.relations.toJSON();

        configurationJSON.put("familles", famillesJSON);
        configurationJSON.put("relations", relationsJSON);

        return configurationJSON;
    }

    /**
     * Charger une configuration de simulation.
     *
     * @param file le fichier de sauvegarde
     * @return la configuration de simulation extraite du fichier et null si le
     *         fichier n'est pas valide
     */
    public static Configuration charger(File file) {
        Configuration configuration = null;
        try {
            FileReader reader = new FileReader(file);
            JSONTokener tokener = new JSONTokener(reader);
            JSONObject configurationJSON = new JSONObject(tokener);
            configuration = Configuration.fromJSON(configurationJSON);
        } catch (IOException e) {
            System.err.println("Erreur lors de la lecture du fichier de configuration.");
        }

        return configuration;
    }

    /**
     * Sauvegarder une configuration de simulation.
     *
     * @param file le fichier de sauvegarde (out)
     */
    public void sauvegarder(File file) {
        JSONObject configurationJSON = this.toJSON();

        try {

            // try-with comme en python pour gérer la fermeture automatique du
            // writer
            try (FileWriter writer = new FileWriter(file)) {
                writer.write(configurationJSON.toString());
            }
        } catch (IOException e) {
            System.err.println("Erreur lors de la sauvegarde de la configuration.");
        }
    }
}
