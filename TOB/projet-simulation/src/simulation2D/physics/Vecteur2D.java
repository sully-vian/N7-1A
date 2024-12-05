package simulation2D.physics;

import org.json.JSONObject;

/**
 * La classe <code>Vecteur2D</code> représente un vecteur algébrique en 2D. Elle
 * possède les méthodes classiques pour effectuer des opérations sur les
 * vecteurs.
 *
 * @author Vianney Hervy
 */
public class Vecteur2D {

    /**
     * La coordonnée x du vecteur.
     */
    private double x;

    /**
     * La coordonnée y du vecteur.
     */
    private double y;

    /**
     * Construire un vecteur à partir de ses coordonnées.
     *
     * @param x la coordonnée x
     * @param y la coordonnée y
     */
    public Vecteur2D(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Obtenir la coordonnée x du vecteur.
     *
     * @return la coordonnée x du vecteur
     */
    public double getX() {
        return this.x;
    }

    /**
     * Obtenir la coordonnée y du vecteur.
     *
     * @return la coordonnée y du vecteur
     */
    public double getY() {
        return this.y;
    }

    /**
     * Définir la coordonnée x du vecteur.
     *
     * @param x la nouvelle coordonnée x du vecteur
     */
    public void setX(double x) {
        this.x = x;
    }

    /**
     * Définir la coordonnée y du vecteur.
     *
     * @param y la nouvelle coordonnée y du vecteur
     */
    public void setY(double y) {
        this.y = y;
    }

    /**
     * Définir les coordonnées x et y du vecteur.
     *
     * @param x la nouvelle coordonnée x du vecteur
     * @param y la nouvelle coordonnée y du vecteur
     */
    public void setXY(double x, double y) {
        this.setX(x);
        this.setY(y);
    }

    /**
     * Obtenir le carré de la norme euclidienne du vecteur.
     *
     * @return le carré de la norme euclidienne du vecteur
     */
    public double norm2() {
        return x * x + y * y;
    }

    /**
     * Ajouter un vecteur à ce vecteur.
     *
     * @param v le vecteur à ajouter à ce vecteur
     */
    public void add(Vecteur2D v) {
        this.x += v.x;
        this.y += v.y;
    }

    /**
     * Additionner deux vecteurs. Aucun changement sur celui-ci.
     *
     * @param v le vecteur à additionner à celui-ci
     * @return this + v
     */
    public Vecteur2D plus(Vecteur2D v) {
        return new Vecteur2D(this.x + v.x, this.y + v.y);
    }

    /**
     * Calculer la différence de deux vecteurs. Aucun changement sur celui-ci.
     *
     * @param v le vecteur à soustraire à celui-ci
     * @return this - v
     */
    public Vecteur2D minus(Vecteur2D v) {
        return new Vecteur2D(this.x - v.x, this.y - v.y);
    }

    /**
     * Multiplier le vecteur par un scalaire. Aucun changement sur celui-ci.
     *
     * @param a le scalaire par lequel multiplier le vecteur
     * @return a * this
     */
    public Vecteur2D times(double a) {
        return new Vecteur2D(this.x * a, this.y * a);
    }

    /**
     * Diviser le vecteur par un scalaire. Aucun changement sur celui-ci.
     *
     * @param a le scalaire par lequel diviser le vecteur
     * @return this / a
     */
    public Vecteur2D divide(double a) {
        return this.times(1 / a);
    }

    /**
     * Obtenir le produit scalaire de ce vecteur par v.
     *
     * @param v le vecteur par lequel multiplier ce vecteur
     * @return le produit scalaire de ce vecteur par v
     */
    public double dotProduct(Vecteur2D v) {
        return this.x * v.x + this.y * v.y;
    }

    /**
     * Obtenir le vecteur unitaire colinéaire à ce vecteur et de même sens.
     *
     * @return le vecteur normalisé
     */
    public Vecteur2D normalized() {
        double norm = Math.sqrt(this.norm2());
        return this.divide(norm);
    }

    /**
     * Normaliser le vecteur.
     */
    public void normalize() {
        double norm = Math.sqrt(this.norm2());
        this.x /= norm;
        this.y /= norm;
    }

    /**
     * Obtenir une copie de ce vecteur.
     *
     * @return une copie de ce vecteur
     */
    public Vecteur2D copy() {
        return new Vecteur2D(this.x, this.y);
    }

    /**
     * Obtenir une représentation textuelle du vecteur.
     *
     * @return la représentation textuelle du vecteur
     */
    @Override
    public String toString() {
        return "(" + this.x + ", " + this.y + ")";
    }

    /**
     * Obtenir une représentation JSON du vecteur.
     *
     * @return la représentation JSON du vecteur
     */
    public JSONObject toJSON() {
        JSONObject json = new JSONObject();
        json.put("x", this.x);
        json.put("y", this.y);
        return json;
    }

    /**
     * Construire un vecteur à partir de sa représentation JSON.
     *
     * @param json la représentation JSON du vecteur
     * @return le vecteur construit à partir de la représentation JSON
     */
    public static Vecteur2D fromJSON(JSONObject json) {
        double x = json.getDouble("x");
        double y = json.getDouble("y");
        return new Vecteur2D(x, y);
    }
}
