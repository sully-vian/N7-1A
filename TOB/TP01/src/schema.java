/**
 * Définition d'un schéma particulier
 * 
 * @author Vianney Hervy
 * @version 0.0
 */
public class schema {

    public static void main(String[] args) {
        // 3,2 11,4 6,9
        Point p1 = new Point(3, 2);
        Point p2 = new Point(11, 4);
        Point p3 = new Point(6, 9);

        Segment s1 = new Segment(p1, p2);
        Segment s2 = new Segment(p2, p3);
        Segment s3 = new Segment(p3, p1);

        double x_bar = (p1.getX() + p2.getX() + p3.getX()) / 3;
        double y_bar = (p1.getY() + p2.getY() + p3.getY()) / 3;

        Point bar = new Point(x_bar, y_bar);

        bar.afficher();
    }
}
