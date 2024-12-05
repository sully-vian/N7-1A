package simulation2D.objects;

import java.util.HashMap;

/**
 * La classe <code>Dico2D</code> permet de créer un dictionnaire dont les clés
 * sont les familles et les valeurs sont des dictionnaires dont les
 * clés sont les familles et les valeurs des flottants de précision
 * double. Cette classe permet de manipuler facilement les relations entre les
 * familles.
 *
 * @author Thomas SABATIER & Vianney HERVY
 */
@Deprecated
public class Dico2DVieux {

    /**
     * Le dictionnaire contenant les relations entre les familles.
     */
    private HashMap<Famille, HashMap<Famille, Double>> dico;

    /**
     * Constructeur de la classe <code>Dico2D</code>.
     */
    public Dico2DVieux() {
        this.dico = new HashMap<Famille, HashMap<Famille, Double>>();
    }

    /**
     * Obtenir le nombre de familles dans le dictionnaire.
     *
     * @return le nombre de familles dans le dictionnaire
     */
    public int getNbFamilles() {
        return this.dico.size();
    }

    /**
     * Modifier la relation de fa sur fb avec une intensité donnée.
     *
     * @param fa        la famille influente
     * @param fb        la famille influencée
     * @param intensite la nouvelle intensité de la relation
     */
    public void modifierRelation(Famille fa, Famille fb, double intensite) {
        if (dico.containsKey(fa)) {
            dico.get(fa).put(fb, intensite);
        } else {
            HashMap<Famille, Double> dicoFa = new HashMap<Famille, Double>();
            dicoFa.put(fb, intensite);
            dico.put(fa, dicoFa);
        }
    }

    /**
     * Supprimer la relation de fa sur fb.
     *
     * @param fa la famille influente
     * @param fb la famille influencée
     */
    public void supprimerRelation(Famille fa, Famille fb) {
        if (dico.containsKey(fa)) {
            dico.get(fa).put(fb, 0.0);
        }
    }

    /**
     * Obtenir l'intensité de la relation de fa sur fb.
     *
     * @param fa la famille influente
     * @param fb la famille influencée
     * @return l'intensité de la relation de fa sur fb
     */
    public double getIntensite(Famille fa, Famille fb) {
        if (dico.containsKey(fa)) {
            return this.dico.get(fa).get(fb);
        }
        return 0;
    }
}
