/**
 * AfficheurTexte
 */
public class AfficheurTexte implements afficheur.Afficheur {

    public void dessinerPoint(double x, double y, java.awt.Color c) {
        System.out.println("Point {");
        afficherDouble("x", x);
        afficherDouble("y", y);
        afficherCouleur(c);
        System.out.println("}");
    }

    public void dessinerLigne(double x1, double y1, double x2, double y2, java.awt.Color c) {
        System.out.println("Ligne {");
        afficherDouble("x1",x1);
        afficherDouble("y1",y1);
        afficherDouble("x2",x2);
        afficherDouble("y2",y2);
        afficherCouleur(c);
        System.out.println("}");
    }

    public void dessinerCercle(double x, double y, double rayon, java.awt.Color c) {
        System.out.println("Cercle {");
        afficherDouble("centre_x", x);
        afficherDouble("centre_y", y);
        afficherDouble("rayon", rayon);
        afficherCouleur(c);
        System.out.println("}");
    }

    public void dessinerTexte(double x, double y, java.lang.String texte, java.awt.Color c) {
        System.out.println("Texte {");
        afficherDouble("x", x);
        afficherDouble("y", y);
        afficherTexte(texte);
        afficherCouleur(c);
        System.out.println("}");
    }

    private void afficherDouble(String nom, double valeur) {
        String doubleString = String.format("\t%s = %s", nom, Double.toString(valeur));
        System.out.println(doubleString);
    }

    private void afficherCouleur(java.awt.Color c) {
        String couleurString = String.format("\tcouleur = %s", c.toString());
        System.out.println(couleurString);
    }

    private void afficherTexte(String valeur) {
        String texteString = String.format("\tvaleur = \"%s\"", valeur);
        System.out.println(texteString);
    }

}