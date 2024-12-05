package simulation2D.objects;

import java.awt.Graphics2D;
import java.awt.Color;

import simulation2D.physics.Vecteur2D;

/**
 * La classe <code>Particule</code> représente une particule simulé dans un
 * espace 2D. Cette particule est caractérisé par sa position, sa masse, son
 * rayon, sa couleur et sa vitesse. Elle appartient à une famille de particules
 * dont elle hérite sa masse, sa couleur et son rayon.
 *
 * @author Vianney Hervy
 */
public class Particule {

    /**
     * La position de la particule.
     */
    private Vecteur2D position;

    /**
     * La masse de la particule.
     */
    private double mass;

    /**
     * Le rayon de la particule.
     */
    private double radius;

    /**
     * La couleur de la particule.
     */
    private Color color;

    /**
     * La vitesse de la particule.
     */
    private Vecteur2D velocity;

    /**
     * Le vecteur force résultant de toutes les forces appliquées sur la particule.
     */
    private Vecteur2D force;

    /**
     * La famille de la particule.
     */
    private Famille famille;

    /**
     * Créer une nouvelle particule.
     *
     * @param famille  la famille de la particule
     * @param position la position initiale de la particule
     * @param velocity la vitesse initiale de la particule
     */
    public Particule(Famille famille, Vecteur2D position, Vecteur2D velocity) {
        this.position = position.copy();
        this.velocity = velocity.copy();
        this.mass = famille.getMass();
        this.radius = famille.getRadius();
        this.color = famille.getColor();
        this.famille = famille;
        this.force = new Vecteur2D(0, 0);
    }

    /**
     * Obtenir la famille de la particules.
     *
     * @return la famille de la particule
     */
    public Famille getFamille() {
        return this.famille.copy();
    }

    /**
     * Obtenir la position x de la particule.
     *
     * @return la position x de la particule
     */
    public double getX() {
        return this.position.getX();
    }

    /**
     * Obtenir la position y de la particule.
     *
     * @return la position y de la particule
     */
    public double getY() {
        return this.position.getY();
    }

    /**
     * Obtenir la position de la particule sous forme de vecteur.
     *
     * @return le vecteur position de la particule
     */
    public Vecteur2D getPosition() {
        return this.position.copy();
    }

    /**
     * Définir la position de la particule par vecteur.
     *
     * @param v le nouveau vecteur position de la particule
     */
    public void setPosition(Vecteur2D v) {
        this.position = v.copy();
    }

    /**
     * Définir la position de la particule par coordonnées.
     *
     * @param x la coordonnée x de la nouvelle position
     * @param y la coordonnée y de la nouvelle position
     */
    public void setPosition(double x, double y) {
        this.setPosition(new Vecteur2D(x, y));
    }

    /**
     * Translater la particule selon un vecteur.
     *
     * @param v le vecteur de translation
     */
    public void translate(Vecteur2D v) {
        this.position.add(v);
    }

    /**
     * Obtenir la masse de la particule.
     *
     * @return la masse de la particule
     */
    public double getMass() {
        return this.mass;
    }

    /**
     * Obtenir le rayon de la particule.
     *
     * @return le rayon de la particule
     */
    public double getRadius() {
        return this.radius;
    }

    /**
     * Définir la position du bord gauche de la particule.
     *
     * @param leftX la nouvelle position du bord gauche de la particule
     */
    public void setLeft(double leftX) {
        this.position.setX(leftX + radius);
    }

    /**
     * Obtenir la position du bord gauche de la particule.
     *
     * @return la position du bord gauche de la particule
     */
    public double getLeft() {
        return position.getX() - radius;
    }

    /**
     * Définir la position du bord droit de la particule.
     *
     * @param rightX la nouvelle position du bord droit de la particule
     */
    public void setRight(double rightX) {
        this.position.setX(rightX - radius);
    }

    /**
     * Obtenir la position du bord droit de la particule.
     *
     * @return la position du bord droit de la particule
     */
    public double getRight() {
        return position.getX() + radius;
    }

    /**
     * Définir la position du bord supérieur de la particule.
     *
     * @param topY la nouvelle position du bord supérieur de la particule
     */
    public void setTop(double topY) {
        this.position.setY(topY + radius);
    }

    /**
     * Obtenir la position du bord supérieur de la particule.
     *
     * @return la position du bord supérieur de la particule
     */
    public double getTop() {
        return position.getY() - radius;
    }

    /**
     * Définir la position du bord inférieur de la particule.
     *
     * @param bottomY la nouvelle position du bord inférieur de la particule
     */
    public void setBottom(double bottomY) {
        this.position.setY(bottomY - radius);
    }

    /**
     * Obtenir la position du bord inférieur de la particule.
     *
     * @return la position du bord inférieur de la particule
     */
    public double getBottom() {
        return position.getY() + radius;
    }

    /**
     * Obtenir le vecteur vitesse de la particule.
     *
     * @return le vecteur vitesse de la particule
     */
    public Vecteur2D getVelocity() {
        return this.velocity.copy();
    }

    /**
     * Définir la vitesse de la particule par vecteur.
     *
     * @param v le nouveau vecteur vitesse de la particule
     */
    public void setVelocity(Vecteur2D v) {
        this.velocity = v.copy();
    }

    /**
     * Définir la vitesse de la particule par coordonnées.
     *
     * @param x la coordonnée x du nouveau vecteur vitesse
     * @param y la coordonnée y du nouveau vecteur vitesse
     */
    public void setVelocity(double x, double y) {
        setVelocity(new Vecteur2D(x, y));
    }

    /**
     * Ajouter une vitesse à la vitesse de la particule.
     *
     * @param v le vecteur vitesse à ajouter à la vitesse de la particule
     */
    public void addVelocity(Vecteur2D v) {
        this.velocity.add(v);
    }

    /**
     * Obtenir la résultante des forces agissant sur la particule.
     *
     * @return la résultante des forces agissant sur la particule
     */
    public Vecteur2D getForce() {
        return this.force.copy();
    }

    /**
     * Définir le vecteur résultante des forces agissant sur la particule.
     *
     * @param v la nouvelle résultante des forces agissant sur la particule
     */
    public void setForce(Vecteur2D v) {
        this.force = v.copy();
    }

    /**
     * Ajouter une force à laquelle la particule est soumis.
     *
     * @param v la nouvelle force auquel la particule est soumis
     */
    public void addForce(Vecteur2D v) {
        this.force.add(v);
    }

    /**
     * Réinitialiser la résultante des forces agissant sur la particule au vecteur
     * nul.
     */
    public void resetForce() {
        this.force = new Vecteur2D(0, 0);
    }

    /**
     * Dessiner la particule.
     *
     * @param g le contexte graphique dans lequel dessiner la particule
     */
    public void draw(Graphics2D g) {
        int topLeftX = (int) (this.getX() - this.radius);
        int topLeftY = (int) (this.getY() - this.radius);
        g.setColor(this.color);
        g.fillOval(topLeftX, topLeftY, (int) radius * 2, (int) radius * 2);
    }

    /**
     * Vérifier si la particule chevauche une autre particule.
     *
     * @param other l'autre particule avec laquel vérifier le chevauchement
     * @return la distance entre les deux particules s'ils se chevauchent, -1 sinon
     */
    public double isOverlapping(Particule other) {
        double dx = this.getX() - other.getX();
        double dy = this.getY() - other.getY();
        double distance2 = dx * dx + dy * dy;
        boolean isOverlapping = distance2 < (this.radius + other.radius) * (this.radius + other.radius);
        return isOverlapping ? Math.sqrt(distance2) : -1;
    }
}
