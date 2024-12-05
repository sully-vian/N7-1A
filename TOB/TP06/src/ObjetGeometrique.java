import java.awt.Color;

/**
 * classe ObjetGeometrique abstraite pour être type d'un élément affichable
 */
abstract public class ObjetGeometrique {

    private Color couleur;

    public ObjetGeometrique(Color couleur) {
        this.couleur = couleur;
    }

    abstract public void afficher();

    abstract public void dessiner(afficheur.Afficheur afficheur);

    abstract public void translater(double dx, double dy);

    public void setCouleur(Color couleur) {
        this.couleur = couleur;
    }

    public Color getCouleur() {
        return this.couleur;
    }
}
