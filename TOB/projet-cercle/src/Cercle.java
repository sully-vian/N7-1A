import java.awt.Color;

/**
 * Cercle modélise un cercle géométrique dans un plan équipé d'un
 * repère cartésien. Un cercle peut être affiché et translaté.
 * On peut obtenir si un point est dans le disque.
 *
 * @author Vianney Hervy <Prénom.Nom@enseeiht.fr>
 */
public class Cercle implements Mesurable2D {

    /**
     * La constante PI.
     */
    public static final double PI = Math.PI;

    /**
     * Le centre du cercle.
     */
    private Point centre;
    /**
     * Le rayon du cercle.
     */
    private double rayon;
    /**
     * La couleur du cercle.
     */
    private Color couleur;

    /**
     * Construire un cercle à partir de son centre et de son rayon. Le cercle est
     * bleu.
     *
     * @param centre centre
     * @param rayon  rayon, strictement positif
     */
    public Cercle(Point centre, double rayon) {
        assert (rayon > 0) : "Le rayon doit être strictement positif";
        assert (centre != null) : "Le centre ne peut pas être null.";

        this.centre = new Point(centre.getX(), centre.getY());
        this.centre.setCouleur(centre.getCouleur());
        this.rayon = rayon;
        this.couleur = Color.blue;
    }

    /**
     * Construire un cercle à partir de deux points diamétralement opposés.
     *
     * @param point1  point 1
     * @param point2  point 2, différent de point1
     * @param couleur couleur
     */
    public Cercle(Point point1, Point point2, Color couleur) {
        assert (point1 != null && point2 != null && couleur != null)
                : "Les arguments ne doivent pas être null.";
        assert (point1.getX() != point2.getX() || point1.getY() != point2.getY())
                : "Les points ne peuvent pas être identiques";

        double cX = (point1.getX() + point2.getX()) / 2.0;
        double cY = (point1.getY() + point2.getY()) / 2.0;
        this.centre = new Point(cX, cY);
        this.rayon = point1.distance(point2) / 2;
        this.couleur = couleur;
    }

    /**
     * Construire un cercle à partir de deux points diamétralement opposés. Le
     * cercle est bleu.
     *
     * @param point1 point 1
     * @param point2 point 2, différent de point1
     */
    public Cercle(Point point1, Point point2) {
        this(point1, point2, Color.blue);
    }

    /**
     * Obtenir le centre du cercle.
     *
     * @return centre du cercle
     */
    public Point getCentre() {
        Point copieCentre = new Point(this.centre.getX(), this.centre.getY());
        return copieCentre;
    }

    /**
     * Obtenir le rayon du cercle.
     *
     * @return rayon du cercle
     */
    public double getRayon() {
        return this.rayon;
    }

    /**
     * Obtenir la couleur du cercle.
     *
     * @return couleur du cercle
     */
    public Color getCouleur() {
        return this.couleur;
    }

    /**
     * Obtenir le diamètre du cercle.
     *
     * @return diamètre du cercle
     */
    public double getDiametre() {
        return this.rayon * 2.0;
    }

    /**
     * Obtenir le périmètre du cercle.
     *
     * @return périmètre du cercle
     */
    public double perimetre() {
        return 2.0 * PI * this.rayon;
    }

    /**
     * Obtenir l'aire du cercle.
     *
     * @return aire du cercle
     */
    public double aire() {
        return PI * this.rayon * this.rayon;
    }

    /**
     * Savoir si un point est à l'intérieur (au sens large) du cercle.
     *
     * @param point point dont on veut connaitre l'appartenance au cercle
     * @return true si le point est dans le cercle
     */
    public boolean contient(Point point) {
        assert (point != null) : "Le point ne doit pas être null.";
        return (this.centre.distance(point) <= this.rayon);
    }

    /**
     * Créer un cercle à partir de son centre et d'un point de sa circonférence. Le
     * cercle est bleu.
     *
     * @param centre centre du cercle
     * @param point  point de sa circonférence, différent de centre
     * @return cercle ainsi créé
     */
    public static Cercle creerCercle(Point centre, Point point) {
        assert (centre != null && point != null)
                : "Le centre et le point ne doit pas être null.";
        assert (centre.getX() != point.getX() || centre.getY() != point.getY())
                : "Le point de la circonférence ne peut être identique au centre";
        double rayon = centre.distance(point);
        return new Cercle(centre, rayon);
    }

    /**
     * Changer le rayon du cercle.
     *
     * @param rayon nouveau rayon, strictement positif
     */
    public void setRayon(double rayon) {
        assert rayon > 0 : "Le rayon doit être strictement positif.";

        this.rayon = rayon;
    }

    /**
     * Changer le diamètre du cercle.
     *
     * @param diametre nouveau diamètre, strictement positif
     */
    public void setDiametre(double diametre) {
        assert diametre > 0 : "Le diamètre doit être strictement positif.";

        this.rayon = diametre / 2.0;
    }

    /**
     * Changer la couleur du cercle.
     *
     * @param couleur nouvelle couleur
     */
    public void setCouleur(Color couleur) {
        assert (couleur != null) : "La couleur ne doit pas être null.";
        this.couleur = couleur;
    }

    /**
     * Translater le cercle.
     *
     * @param dx déplacement suivant l'axe des X
     * @param dy déplacement suivant l'axe des Y
     */
    public void translater(double dx, double dy) {
        this.centre.translater(dx, dy);
    }

    /**
     * Obtenir une représentation textuelle du cercle.
     *
     * @return représentation textuelle du cercle de la forme "C(rayon)@(centre)"
     */
    public String toString() {
        return "C" + this.rayon + "@" + this.centre;
    }

    /**
     * Aficher le cercle.
     */
    public void afficher() {
        System.out.print(this);
    }
}
