package simulation2D.menu;

import simulation2D.Launch;
import simulation2D.objects.Particule;
import simulation2D.objects.Famille;
import simulation2D.objects.DicoRelations2D;

import java.util.ArrayList;
import java.util.List;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.FlowLayout;
import java.awt.Dimension;
import java.awt.GridLayout;

import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

/**
 * Classe gérant le menu graphique.
 */
@Deprecated
public class MenuGeneral extends JFrame {
    /**
     * Les boutons du menu général.
     */
    private JButton boutonAjouter, boutonAfficher, boutonStart;

    /**
     * Les particules.
     */
    private List<Particule> particules;

    /**
     * Les relations entre les familles de particules.
     */
    private DicoRelations2D relations;

    /**
     * Les familles de particules.
     */
    private List<Famille> familles;

    /**
     * La visibilité du menu.
     * TODO: trouver un moyen de cacher le menu après le lancement
     */
    private Boolean visible = true;

    /**
     * Créer un menu général pour l'application.
     */
    public MenuGeneral() {
        this.setTitle("Simulation");
        this.setSize(700, 200);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setLocationRelativeTo(null);
        this.getContentPane().setLayout(new FlowLayout());
        // Initialisation des listes des familles
        this.familles = new ArrayList<>();
        this.particules = new ArrayList<>();

        // Sous menu permettant d'ajouter des familles
        boutonAjouter = new JButton("Ajouter une famille");
        boutonAjouter.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                // SousMenu sousMenu = new SousMenu(null, "Ajouter une famille", true);
                // familles.addAll(sousMenu.afficherFenetreAjout());
                // TODO : c'est tres bizarre que sousmenu gère les relations (et elle le fait
                // pas d'ailleurs) donc a modif et faut générer les relations autre part que
                // dans ce sous menu
                // relations = sousMenu.getRelations();
                // particules.addAll(sousMenu.getParticules());
            }
        });

        // Sous menu permettant d'afficher les familles
        boutonAfficher = new JButton("Afficher les familles");
        boutonAfficher.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                afficheFamilles();
            }
        });

        // Bouton démarrant la simulation
        boutonStart = new JButton("Démarrer la simulation");
        boutonStart.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                Launch.launch(null);
                visible = false;
            }
        });

        this.getContentPane().add(boutonAjouter);
        this.getContentPane().add(boutonAfficher);
        this.getContentPane().add(boutonStart);
        this.setVisible(true);
    }

    /**
     * Obtenir les familles de particules.
     *
     * @return les familles de particules
     */
    public List<Famille> getFamilles() {
        return this.familles;
    }

    /**
     * Obtenir les particules de la simulation.
     *
     * @return les particules de la simulation
     */
    public List<Particule> getParticules() {
        return this.particules;
    }

    /**
     * Obtenir les relations entre les familles de particules.
     *
     * @return les relations entre les familles de particules
     */
    public DicoRelations2D getRelations() {
        return this.relations;
    }

    /**
     * Afficher les familles de particules.
     */
    public void afficheFamilles() {
        JPanel panelFamilles = new JPanel(new GridLayout(familles.size(), 2));

        for (int i = 0; i < familles.size(); i++) {
            Famille famille = familles.get(i);
            JLabel labelFamille = new JLabel("<html>" + famille.toString().replace("\n", "<br>") + "</html>");
            JButton boutonSupprimer = new JButton("Supprimer");
            boutonSupprimer.setPreferredSize(new Dimension(40, 25));
            boutonSupprimer.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent arg0) {
                    // familles.remove(famille);
                    afficheFamilles();
                }
            });

            panelFamilles.add(labelFamille);
            panelFamilles.add(boutonSupprimer);

        }

        JOptionPane.showMessageDialog(null, panelFamilles, "Familles ajoutées", JOptionPane.PLAIN_MESSAGE);
    }

    /**
     * Lancer le menu général.
     */
    public static void main() {
        MenuGeneral menuGeneral = new MenuGeneral();
    }
}
