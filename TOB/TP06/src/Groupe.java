import java.awt.Color;
import java.util.ArrayList;

public class Groupe extends ObjetGeometrique {
    // extends ObjetGeometrique, pour pouvoir être dans un groupe lui-même

    private ArrayList<ObjetGeometrique> groupe;

    /**
     * Constructeur initialisant un groupe vide.
     */
    public Groupe() {
        super(Color.green);
        this.groupe = new ArrayList<ObjetGeometrique>();
    }

    public void ajouter(ObjetGeometrique objet) {
        this.groupe.add(objet);
    }

    public void supprimer(ObjetGeometrique objet) {
        this.groupe.remove(objet);
    }

    public void translater(double dx, double dy) {
        for (ObjetGeometrique objet : this.groupe) {
            objet.translater(dx, dy);
        }
    }

    public void afficher() {
        for (ObjetGeometrique objet : this.groupe) {
            objet.afficher();
            System.out.println();
        }
    }

    public void dessiner(afficheur.Afficheur afficheur) {
        for (ObjetGeometrique objet : this.groupe) {
            objet.dessiner(afficheur);
        }
    }
}
