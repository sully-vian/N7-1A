/**
 * Définition d'un ensemble chainé d'entiers.
 */
public class EnsembleChaine implements Ensemble {

    private int cardinal;
    private Cellule elements;

    public EnsembleChaine() {
        this.cardinal = 0;
        this.elements = null;
    }

    public int cardinal() {
        return this.cardinal;
    }

    public boolean estVide() {
        return (this.elements == null);
    }

    public boolean contient(int x) {
        Cellule cell = this.elements;
        boolean contient = false;

        while (!contient && cell != null) {
            contient = (cell.contenu == x);
            cell = cell.suivant;
        }
        return contient;
    }

    public void ajouter(int x) {
        Cellule cell = this.elements;
        boolean contient = false;

        while (!contient) {
            if (cell == null) {
                cell = new Cellule(x);
                this.cardinal++;
                contient = false;
            } else {
                contient = (cell.contenu == x);
                cell = cell.suivant;
            }
        }
    }

    public void supprimer(int x) {
        Cellule cell = this.elements;
        boolean stop = false;

        while (!stop && cell != null) {
            if (cell.contenu == x) {
                if (cell.suivant == null) {
                    cell = null;
                } else {
                    cell.suivant = cell.suivant.suivant;
                }

                stop = true;
            } else {
                cell = cell.suivant;
            }
        }
    }

    private class Cellule {

        int contenu;
        Cellule suivant;

        Cellule(int contenu_) {
            this.contenu = contenu_;
            this.suivant = null;
        }
    }

}
