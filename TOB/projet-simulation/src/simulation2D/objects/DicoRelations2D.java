package simulation2D.objects;

import java.util.HashMap;
import java.util.Set;

import org.json.JSONObject;
import org.json.JSONException;

/**
 * La classe <code>DicoRelations2D</code> permet de créer un dictionnaire dont
 * les clés sont les noms des <code>Famille</code> et les valeurs sont des
 * <code>DicoRelations</code>. Cette classe permet de manipuler facilement les
 * relations entre les familles.
 *
 * @author Thomas SABATIER & Vianney HERVY
 */
public class DicoRelations2D extends HashMap<String, DicoRelations> {

    /**
     * Construire un dictionnaire vide.
     */
    public DicoRelations2D() {
        super();
    }

    /**
     * Obtenir les noms des familles.
     *
     * @return les noms des familles
     */
    public Set<String> getNomsFamilles() {
        return this.keySet();
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
     * Obtenir le dictionnaire des relations sur une famille.
     *
     * @param familleInfluencee la famille influencée
     * @return le dictionnaire des relations sur la famille donnée
     */
    public DicoRelations getRelations(Famille familleInfluencee) {
        return this.getRelations(familleInfluencee.getNom());
    }

    /**
     * Obtenir le dictionnaire des relations sur une famille à partir de son
     * nom.
     *
     * @param nomFamille le nom de la famille influencée
     * @return le dictionnaire des relations sur la famille
     */
    public DicoRelations getRelations(String nomFamille) {
        return this.get(nomFamille);
    }

    /**
     * Obtenir l'intensité de la relation d'une famille sur l'autre.
     *
     * @param familleInfluencee la famille influencée
     * @param familleInfluente  la famille influente
     * @return l'intensité de la relation de la famille influente sur la famille
     *         influencée
     */
    public double getIntensite(Famille familleInfluencee, Famille familleInfluente) {
        String nomInfluente = familleInfluente.getNom();
        String nomInfluencee = familleInfluencee.getNom();
        return getIntensite(nomInfluencee, nomInfluente);
    }

    /**
     * Obtenir l'intensité de la relation d'une famille sur l'autre à partir de
     * leurs noms.
     *
     * @param nomInfluencee le nom de la famille influencée
     * @param nomInfluente  le nom de la famille influente
     * @return l'intensité de la relation de la famille influente sur la famille
     *         influencée
     */
    public double getIntensite(String nomInfluencee, String nomInfluente) {
        DicoRelations relationsInfluencee = this.getRelations(nomInfluencee);
        return relationsInfluencee.getIntensite(nomInfluente);
    }

    /**
     * Définir l'intensité de la relation d'une famille sur l'autre.
     *
     * @param familleInfluencee la famille influencée
     * @param familleInfluente  la famille influente
     * @param intensite         la nouvelle intensité de la relation
     */
    public void setRelation(Famille familleInfluencee, Famille familleInfluente, double intensite) {
        String nomInfluente = familleInfluente.getNom();
        String nomInfluencee = familleInfluencee.getNom();
        setRelation(nomInfluencee, nomInfluente, intensite);
    }

    /**
     * Définir l'intentité de la relation d'une famille sur l'autre avec leurs
     * noms.
     *
     * @param nomInfluencee le nom de la famille influencée
     * @param nomInfluente  le nom de la famille influente
     * @param intensite     la nouvelle intensité de la relation
     */
    public void setRelation(String nomInfluencee, String nomInfluente, double intensite) {
        DicoRelations relationsInfluencee;
        if (this.containsKey(nomInfluencee)) {
            relationsInfluencee = this.getRelations(nomInfluencee);
            relationsInfluencee.setIntensite(nomInfluente, intensite);
        } else {
            relationsInfluencee = new DicoRelations();
            relationsInfluencee.setIntensite(nomInfluente, intensite);
        }
    }

    /**
     * Créer les relations entre la famille donnée et les autres familles. Les
     * intensités de ces relations sont initialisées à 0.
     *
     * @param famille la famille dont on veut créer les relations
     */
    public void creerRelations(Famille famille) {
        String nomFamille = famille.getNom();
        creerRelations(nomFamille);
    }

    public void creerRelations(String nomFamille) {
        DicoRelations relationsFamille = new DicoRelations();
        for (String nomAutre : this.getNomsFamilles()) {
            // relations des autres familles sur celle donnée
            relationsFamille.setIntensite(nomAutre, 0);

            // relations de la famille donnée sur les autres
            this.getRelations(nomAutre).setIntensite(nomFamille, 0);
        }

        this.put(nomFamille, relationsFamille);
    }

    /**
     * Supprimer toutes les relations impliquant la famille donnée.
     *
     * @param famille la famille dont on veut supprimer les relations
     */
    public void supprimerRelations(Famille famille) {
        String nomFamille = famille.getNom();
        supprimerRelation(nomFamille);
    }

    /**
     * Supprimer toutes les relations impliquant la famille du nom donné.
     *
     * @param nomFamille le nom de la famille dont on veut supprimer les
     *                   relations
     */
    public void supprimerRelation(String nomFamille) {
        // relations des autres familles sur celle donnée
        this.remove(nomFamille);

        // relations de la famille donnée sur les autres
        for (String nomInfluencee : this.getNomsFamilles()) {
            this.getRelations(nomInfluencee).supprimerRelation(nomFamille);
        }
    }

    /**
     * Construire la représentation JSON du dictionnaire.
     *
     * @return la représentation JSON du dictionnaire
     * @throws JSONException si une erreur survient lors de la construction du
     *                       JSON
     */
    public JSONObject toJSON() throws JSONException {
        JSONObject jsonGlobal = new JSONObject();
        for (String nomInfluencee : this.getNomsFamilles()) {
            DicoRelations relationsInfluencee = this.getRelations(nomInfluencee);
            JSONObject jsonInfluencee = relationsInfluencee.toJSON();
            jsonGlobal.put(nomInfluencee, jsonInfluencee);
        }

        return jsonGlobal;
    }

    /**
     * Construire un dictionnaire à partir de sa représentation JSON.
     *
     * @param json la représentation JSON du dictionnaire
     * @return le dictionnaire
     * @throws JSONException si une erreur survient lors de la lecture du JSON
     */
    public static DicoRelations2D fromJSON(JSONObject json) throws JSONException {
        DicoRelations2D relations = new DicoRelations2D();

        for (String nomInfluencee : json.keySet()) {
            JSONObject jsonInfluencee = json.getJSONObject(nomInfluencee);
            DicoRelations relationsInfluencee = DicoRelations.fromJSON(jsonInfluencee);
            relations.put(nomInfluencee, relationsInfluencee);
        }

        return relations;
    }

}
