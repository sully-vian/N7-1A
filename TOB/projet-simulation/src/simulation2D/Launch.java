package simulation2D;

import javax.swing.JFrame;
import javax.swing.JPanel;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.WindowAdapter;

import java.util.ArrayList;
import java.util.List;

import java.awt.Color;
import java.awt.Dimension;

import simulation2D.objects.Particule;
import simulation2D.objects.Famille;
import simulation2D.physics.Vecteur2D;
import simulation2D.rendering.Affichage;
import simulation2D.simulation.SimulationLoop;
import simulation2D.simulation.SimulationState;
import simulation2D.simulation.Configuration;
import simulation2D.menu.MenuQuitter;

/**
 * La classe <code>Launch</code> est le point d'entrée de l'application. Elle
 * initialise l'application et lance la boucle de simulation.
 *
 * @author Vianney Hervy
 */
public class Launch {

    /**
     * Le nombre d'images par seconde de la simulation. Cette valeur concerne
     * l'affichage tout comme le calcul des positions par le moteur physique.
     */
    public static final int FPS = 60;

    /**
     * La largeur de la fenêtre de l'application et de l'espace de simulation.
     */
    public static final int WIDTH = 800;

    /**
     * La hauteur de la fenêtre de l'application et de l'espace de simulation.
     */
    public static final int HEIGHT = 600;

    /**
     * La configuration de la simulation.
     */
    private static Configuration configuration;

    public static JFrame frame;

    /**
     * Lancer la simulation.
     *
     * @param configuration la configuration de la simulation
     */
    public static void launch(Configuration configuration) {

        Launch.configuration = configuration;

        List<Particule> particules = genererParticules();

        // Créer l'étatde la simulation
        SimulationState simulationState = new SimulationState(particules, configuration);

        // Créer moteur de rendu
        final Affichage renderer = new Affichage(simulationState);

        // Créer la fenêtre de l'application
        Launch.frame = new JFrame("Simulation 2D");
        Launch.frame.setResizable(false);
        Launch.frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        Launch.frame.setSize(WIDTH, HEIGHT);
        Launch.frame.setVisible(true);

        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent windowEvent) {
                new MenuQuitter(Launch.configuration, Launch.frame);
            }
        });

        // ajouter un panneau de dessin à la fenêtre
        JPanel panel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);

                renderer.render((Graphics2D) g);
            }
        };
        panel.setBackground(Color.LIGHT_GRAY);
        panel.setPreferredSize(new Dimension(WIDTH, HEIGHT));
        frame.add(panel);
        frame.pack();

        // Créer la boucle de simulation
        SimulationLoop simulationLoop = new SimulationLoop(simulationState, panel);

        // Lancer la boucle de smulation
        simulationLoop.start();
    }

    private static List<Particule> genererParticules() {
        List<Particule> particules = new ArrayList<>();
        for (Famille famille : Launch.configuration.getFamilles()) {
            for (int i = 0; i < famille.getNbParticules(); i++) {
                double posX = Math.random() * Launch.WIDTH;
                double posY = Math.random() * Launch.HEIGHT;
                particules.add(new Particule(famille, new Vecteur2D(posX, posY), famille.getVitesseInit()));
            }
        }
        return particules;
    };

    /**
     * Constructeur privé pour empêcher l'instanciation de la classe.
     */
    private Launch() {
    }
}
