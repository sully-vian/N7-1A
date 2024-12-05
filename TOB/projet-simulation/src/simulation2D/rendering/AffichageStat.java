package simulation2D.rendering;

import java.awt.*;
import javax.swing.*;

import simulation2D.Launch;
import simulation2D.statistiques.EnergieCinetique;
//import simulation2D.Launch;
import simulation2D.statistiques.Statistique;

//import java.awt.geom.Line2D;
import java.util.ArrayList;
//import java.util.Collection;
import java.util.List;

class StatCanva extends JComponent {
    Statistique stat;

    public void paint(Graphics graphics) {

        /**
         * Dessiner le graphique.
         *
         * @param graphics le contexte graphique
         */

        super.paintComponent(graphics);

        // --- White background ---
        graphics.setColor(Color.WHITE);
        graphics.fillRect(0, 0, getWidth(), getHeight());

        // --- Draw axis ---
        graphics.setColor(Color.GRAY);
        graphics.drawLine( 3, (int) (getHeight()-3),(int) (getWidth()-3), (int) (getHeight()-3) );   //axe x
        graphics.drawLine( 3, 3, 3, (int) (getHeight()-3));  //axe y

        // --- Draw values ---
        graphics.setColor(Color.BLACK);
        graphics.drawString("0,0", (int) (getWidth() * 0.02), (int) (getHeight() * 0.92));
        graphics.drawString("temps (s)", (int) (getWidth() * 0.90), (int) (getHeight() * 0.92));

        // --- Draw curve ---
        List<Double> data = stat.getData();
        // -- Courbe rouge si Energie cinétique
        if (stat instanceof EnergieCinetique) {
            graphics.setColor(Color.RED);
            graphics.drawString("Energie Cinetique", (int) (getWidth() * 0.75), (int) (getHeight() * 0.2));
        } else {
            graphics.setColor(Color.BLUE);
            graphics.drawString("Collision", (int) (getWidth() * 0.75), (int) (getHeight() * 0.2));
        }

        List<Double> dataMoy = moyenneVal(data);
        int taille =  dataMoy.size();
        double max = max(dataMoy);
        int oldX = xToPixel(0, taille);
        int oldY = yToPixel( dataMoy.get(0) , max);
        for (int i = 1; i < taille-1 ;  i++){
            int dx = xToPixel((double) i, taille );
            int dy = yToPixel( dataMoy.get(i), max );
            graphics.drawLine( dx, dy, oldX, oldY );
            oldX = dx;
            oldY = dy;
        }
    }

    /**
     * Construire un graphique des statistiques données.
     *
     * @param data
     */
    // public StatCanva(Statistique data) {
    // this.donnees = data;
    // }

    public StatCanva(Statistique stat) {
        super();
        this.stat = stat;
    }

    private Double max(List<Double> liste) {
        double max = 0;
        for (Double nb : liste) {
            if (nb > max) {
                max = nb;
            }
        }
        return max;
    }

        private List<Double> moyenneVal(List<Double> liste) {
            List<Double> listeMoy = new ArrayList<>();
            int taille = liste.size();
            if (taille > 100) {
                int q = taille / 100;
                int r = taille % 100;
                for (int i = 0; i<q; i++) {
                    double moy = 0;
                    for (int j = 0; j<100; j++) {
                        moy = moy + liste.get(j);
                    }
                    moy = moy /100;
                    listeMoy.add(i, moy);
                }
                double moy2 = 0;
                for (int i=100*q; i<taille; i++) {
                    moy2 = moy2 + liste.get(i);
                }
                listeMoy.add(moy2/r);
            } else {
                listeMoy = liste;
            }      
            // double max = max(listeMoy);
            // for (Double element : listeMoy) {
            //     element = element / max;
            // }
            //listeMoy.replaceAll(nb -> nb/max);      
            return listeMoy;
        }
        /**
         * Convertir une valeur x en pixel.
         *
         * @param x la valeur x
         * @return la valeur x en pixel
         */
        private int xToPixel( double x , int taille) {
            if (taille > 100) {
                return (int) (getWidth() * x /taille) + 3;
            } else {
                return (int)( getWidth() * x/100) + 3;
            }
        }
    
    
        /**
         * Convertir une valeur y en pixel.
         *
         * @param y la valeur y
         * @return la valeur y en pixel
         */
        private int yToPixel( double y, double max) {
            return (int)(getHeight() *(1-y/max) );     //10000000 valeur arbitraire
        }
}

public class AffichageStat extends JFrame {
    Statistique stat;

    public AffichageStat(Statistique stat, int nb) {
        super("Statistiques");
        // this.data = data;
        this.stat = stat;
        this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);

        this.setSize(550, 500);
        this.setLocationRelativeTo(Launch.frame);
        this.setLocation(this.getLocation().x + Launch.WIDTH-132,this.getLocation().y + nb);
        this.getContentPane().add(new StatCanva(this.stat));
        this.setVisible(true);
    }

    public void update(Statistique stat) {
        this.getContentPane().add(new StatCanva(this.stat));
        this.setVisible(true);
        this.setResizable(false);
    }
}
