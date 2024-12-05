package simulation2D.statistiques;

import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.Timer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import simulation2D.Launch;
import simulation2D.objects.Particule;
import simulation2D.simulation.SimulationState;

/**
 * Gère les statistiques de la densite.
 *
 * @author Yassine Bougacha
 */
public class DensityStat {

    private int updateIteration;
    /**
     * La matrice contenant l'information.
     */
    private double[][] data;

    /**
     * La taille des secteurs.
     */
    private int size;

    /**
     * Le nombre de colonnes.
     */
    private int cols;

    /**
     * Le nombre de lignes.
     */
    private int rows;

    private JFrame frame;

    /**
     * Créer une nouvelle statistique de densité.
     *
     * @param taille la taille des secteurs
     */
    public DensityStat(int taille) {
        this.size = taille;
        this.cols = (int) Launch.WIDTH / taille;
        this.rows = (int) Launch.HEIGHT / taille;
        this.data = new double[rows][cols];
        this.updateIteration = 0;
    }

    /**
     * Mettre à jour les statistiques de densité.
     *
     * @param state l'état de la simulation
     */
    public void updateStat(SimulationState state) {
        List<Particule> particules = state.getParticles();
        int nbParticules = particules.size();
        if (this.updateIteration > Launch.FPS) {
            for (Particule particule : particules) {
                int col = (int) (particule.getX() / size);
                int row = (int) (particule.getY() / size);
                this.data[row][col]++;
            }
            this.updateIteration = 0;
        } else {
            this.updateIteration++;
        }
        devideMatrixByValue(nbParticules);
    }

    /**
     * Obtenir les données de statistique de densité.
     *
     * @return les données de statistique de densité
     */
    public double[][] getData() {
        return this.data;
    }

    /**
     * Afficher les statistiques de densité.
     */
    public void afficher() {
        BufferedImage image = matrixToImage();
        this.frame = new JFrame("Statistique de densite");
        this.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        JLabel label = new JLabel(new ImageIcon(image));
        this.frame.getContentPane().add(label, BorderLayout.CENTER);
        this.frame.pack();
        this.frame.setVisible(true);
        // la fenetre sera fermer automatiquement apres 1 seconde.
        Timer timer = new Timer(1000, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DensityStat.this.frame.dispose();
            }

        });
        timer.setRepeats(false);
        timer.start();
    }

    /**
     * Convertir la matrice en image.
     *
     * @return l'image correspondante
     */
    private BufferedImage matrixToImage() {
        BufferedImage image = new BufferedImage(this.cols, this.rows, BufferedImage.TYPE_INT_RGB);
        for (int i = 0; i < this.rows; i++) {
            for (int j = 0; j < this.cols; j++) {
                Color color = mapValueToColor(this.data[i][j]);
                image.setRGB(i, j, color.getRGB());
            }
        }
        return image;
    }

    /**
     * Mapper une valeur à une couleur.
     *
     * @param value la valeur
     * @return la couleur correspondante
     */
    private static Color mapValueToColor(double value) {
        int red = (int) (255 * value);
        int green = (int) (255 * value);
        int blue = (int) (255 * (1 - value));
        return new Color(red, green, blue);
    }

    /**
     * Diviser la matrice par une valeur (la normaliser avec le nombre total de
     * particules).
     *
     * @param nbParticules le nombre de particules
     */
    private void devideMatrixByValue(int nbParticules) {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                this.data[i][j] /= nbParticules;
            }
        }
    }

}
