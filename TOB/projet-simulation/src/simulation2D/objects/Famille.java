package simulation2D.objects;

import java.awt.Color;
import org.json.JSONObject;

import simulation2D.physics.Vecteur2D;

/**
 * La classe Famille permet de créer une famille de particules dont
 * l'utilisateur va choisir les différentes caractéristiques.
 * a completer
 *
 * @author Leilie CANILLAC
 */

public class Famille {

    /**
     * Le nom de la famille.
     */
    public String nom;

    /**
     * La couleur de la famille.
     */
    private Color color;

    /**
     * La masse de la famille.
     */
    private double mass;

    /**
     * Le rayon de la famille.
     */
    private double radius;

    /**
     * Le nombre de particule.
     */
    private int nbParticule;

    /**
     * La vitesse initiale.
     */
    private Vecteur2D vitesseInit;

    /**
     * Construire une famille dont les particules auront la vitesse initiale donnée.
     *
     * @param nom         le nom de la famille
     * @param color       la couleur de la famille
     * @param mass        la masse de la famille
     * @param radius      le rayon des particules de la famille
     * @param nbParticule le nombre de particules de la famille
     * @param vitesse     la vitesse initiale des particules de la famille
     */
    public Famille(String nom, Color color, double mass, double radius, int nbParticule, Vecteur2D vitesse) {
        this.nom = nom;
        this.color = color;
        this.mass = mass;
        this.radius = radius;
        this.nbParticule = nbParticule;
        this.vitesseInit = vitesse;
    }

    /**
     * Construire une famille dont les particules auront une vitesse initiale
     * nulle.
     *
     * @param nom         le nom de la famille
     * @param color       la couleur de la famille
     * @param mass        la masse de la famille
     * @param radius      le rayon des particules de la famille
     * @param nbParticule le nombre de particules de la famille
     */
    public Famille(String nom, Color color, double mass, double radius, int nbParticule) {
        this(nom, color, mass, radius, nbParticule, new Vecteur2D(0, 0));
    }

    /**
     * Construire une famille par défaut à partir de son nom. Ses attributs seront
     * aléatoires et suivront une loi uniforme sur :
     * masse : [0, 100],
     * rayon : [5, 25],
     * nombre de particules : [0, 100],
     * vitesse initiale : [-100, 100] x [-100, 100],
     *
     * @param nom le nom de la famille
     */
    public Famille(String nom) {
        this.nom = nom;
        this.color = new Color((int) (Math.random() * 0x1000000));
        this.mass = Math.round(Math.random() * 100 * 1000) / 1000.0;
        this.radius = Math.round((Math.random() * 20 + 5) * 1000) / 1000.0;
        this.nbParticule = (int) (Math.random() * 100);
        double vitesseRandX = Math.round((Math.random() - 0.5) * 200 * 1000) / 1000.0;
        double vitesseRandY = Math.round((Math.random() - 0.5) * 200 * 1000) / 1000.0;
        this.vitesseInit = new Vecteur2D(vitesseRandX, vitesseRandY);
    }

    /**
     * Obtenir le nombre de particules de la famille.
     *
     * @return le nombre de particules de la famille
     */
    public int getNbParticules() {
        return this.nbParticule;
    }

    /**
     * Modifier le nombre de particules de la famille.
     *
     * @param nb
     */
    public void setNbParticules(int nb) {
        this.nbParticule = nb;
    }

    /**
     * Obtenir la vitesse initiale de la famille.
     *
     * @return la vitesse initiale de la famille
     */
    public Vecteur2D getVitesseInit() {
        return this.vitesseInit.copy();
    }

    /**
     * Modifier la vitesse initiale de la famille.
     *
     * @param vitesse la nouvelle vitesse initiale de la famille
     */
    public void setVitesseInit(Vecteur2D vitesse) {
        this.vitesseInit = vitesse.copy();
    }

    /**
     * Obtenir le nom de la famille.
     *
     * @return le nom de la famille
     */
    public String getNom() {
        return this.nom;
    }

    /**
     * Modifier le nom de la famille.
     *
     * @param nom le nouveau nom de la famille
     */
    public void setNom(String nom) {
        this.nom = nom;
    }

    /**
     * Obtenir la masse de la famille.
     *
     * @return la masse de la famille
     */
    public double getMass() {
        return this.mass;
    }

    /**
     * Modifier la masse de la famille.
     *
     * @param mass la nouvelle masse de la famille
     */
    public void setMass(double mass) {
        this.mass = mass;
    }

    /**
     * Obtenir le rayon des particules de la famille.
     *
     * @return le rayon des particules de la famille
     */
    public double getRadius() {
        return this.radius;
    }

    /**
     * Modifier le rayon des particules la famille.
     *
     * @param radius le nouveau rayon des particules la famille
     */
    public void setRadius(double radius) {
        this.radius = radius;
    }

    /**
     * Obtenir la couleur de la famille.
     *
     * @return la couleur de la famille.
     */
    public Color getColor() {
        return this.color;
    }

    /**
     * Modifier la couleur de la famille.
     *
     * @param color la nouvelle couleur de la famille
     */
    public void setColor(Color color) {
        this.color = color;
    }

    /**
     * Renvoie une copie de la famille.
     *
     * @return copie de la famille
     */
    public Famille copy() {
        return new Famille(this.nom, this.color, this.mass, this.radius, 0);
    }

    /**
     * Renvoie une chaine de caractère décrivant la famille.
     *
     * @return la chaine de caractère décrivant la famille
     */
    public String toString() {
        String str;
        if (this.nom != null && this.color != null && this.radius != 0 && this.mass != 0 && this.nbParticule != 0) {
            str = "Description de la famille " + this.nom + " " + "\n";
            str += "Rayon : " + this.radius + " \n";
            str += "Masse : " + this.mass + " \n";
            str += "Couleur : " + this.color + " \n";
            str += "Nombre : " + this.nbParticule + " \n";
            str += "Vitesse Initiale : " + this.vitesseInit + " \n";

        } else {
            str = "Aucune information !";
        }
        return str;
    }

    /**
     * Construire la représentation JSON de la famille. Les clefs du JSON sont les
     * attributs de la famille.
     *
     * @return la représentation JSON de la famille
     */
    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        json.put("nom", this.nom);
        json.put("couleur", this.color.getRGB());
        json.put("mass", this.mass);
        json.put("radius", this.radius);
        json.put("nbParticule", this.nbParticule);
        json.put("vitesseInit", this.vitesseInit.toJSON());

        return json;
    }

    /**
     * Construire une famille à partir de sa représentation JSON.
     *
     * @param json la représentation JSON de la famille
     * @return la famille construite à partir de la représentation JSON
     */
    public static Famille fromJSON(JSONObject json) {
        String nom = json.getString("nom");
        Color color = new Color(json.getInt("couleur"));
        double mass = json.getDouble("mass");
        double radius = json.getDouble("radius");
        int nbParticule = json.getInt("nbParticule");
        Vecteur2D vitesseInit = Vecteur2D.fromJSON(json.getJSONObject("vitesseInit"));

        return new Famille(nom, color, mass, radius, nbParticule, vitesseInit);
    }

}
