package simulation2D.objects;

import java.util.HashMap;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * la classe <code>DicoRelations</code> représente le dictionnaire des relations
 * d'autres familles sur une famille donnée. Les clefs sont les
 * <code>Famille</code> influentes et les valeurs sont les intensités des
 * relations.
 *
 * @author Thomas SABATIER & Vianney HERVY
 */
public class DicoRelations extends HashMap<String, Double> {

    /**
     * Construire un dictionnaire de relations vide.
     */
    public DicoRelations() {
        super();
    }

    /**
     * Obtenir le nombre de familles dans le dictionnaire.
     *
     * @return le nombre de familles dans le dictionnaire
     */
    public int getNbFamilles() {
        return this.size();
    }

    /**
     * Obtenir l'intensité de la relation d'une famille sur l'autre.
     *
     * @param familleInfluente la famille influente
     * @return l'intensité de la relation de la famille influente
     */
    public double getIntensite(Famille familleInfluente) {
        return this.getIntensite(familleInfluente.getNom());
    }

    /***
     * Obtenir l'intensité de la relation d'une famille sur l'autre à partir de
     * son nom.
     *
     * @param nomInfluente le nom de la famille influente
     * @return l'intensité de la relation de la famille influente
     */
    public double getIntensite(String nomInfluente) {
        Double val = this.get(nomInfluente);
        if (val == null) {
            return 0;
        }
        return val;
    }

    /**
     * Définir l'intensité de la relation d'une famille sur l'autre.
     *
     * @param familleInfluente la famille influente
     * @param intensite        la nouvelle intensité de la relation
     */
    public void setIntensite(Famille familleInfluente, double intensite) {
        this.setIntensite(familleInfluente.getNom(), intensite);
    }

    /**
     * Définir l'intentité de la relation d'une famille sur l'autre à partir de
     * son nom.
     *
     * @param nomInfluente le nom de la famille influente
     * @param intensite    la nouvelle intensité de la relation
     */
    public void setIntensite(String nomInfluente, double intensite) {
        this.put(nomInfluente, intensite);
    }

    /**
     * Créer une relation avec une famille. Son intensité est initialisée à 0.
     *
     * @param nomInfluente
     */
    public void creerRelation(String nomInfluente) {
        this.put(nomInfluente, 0.0);
    }

    /**
     * Supprimer la relation d'une famille.
     *
     * @param nomInfluente le nom de la famille dont on veut supprimer la
     *                     relation
     */
    public void supprimerRelation(String nomInfluente) {
        this.remove(nomInfluente);
    }

    /**
     * Obtenir la représentation JSON du dictionnaire de relations.
     *
     * @return la représentation JSON du dictionnaire de relations
     * @throws JSONException si une erreur survient lors de l'écriture du JSON
     */
    public JSONObject toJSON() throws JSONException {
        JSONObject json = new JSONObject();
        for (String nomInfluente : this.keySet()) {
            json.put(nomInfluente, this.getIntensite(nomInfluente));
        }
        return json;
    }

    /**
     * Construire un dictionnaire de relations à partir de sa représentation
     * JSON.
     *
     * @param json la représentation JSON du dictionnaire de relations
     * @return le dictionnaire de relations
     * @throws JSONException si une erreur survient lors de la lecture du JSON
     */
    public static DicoRelations fromJSON(JSONObject json) throws JSONException {
        DicoRelations dico = new DicoRelations();
        for (String nomInfluente : json.keySet()) {
            dico.setIntensite(nomInfluente, json.getDouble(nomInfluente));
        }
        return dico;
    }
}
