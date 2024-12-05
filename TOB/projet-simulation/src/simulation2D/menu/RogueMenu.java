package simulation2D.menu;

import simulation2D.simulation.Configuration;

import java.awt.CardLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.File;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JFileChooser;

public class RogueMenu extends JFrame {

    /**
     * Le gestionnaire de disposition des menus.
     */
    protected CardLayout cardLayout;

    /**
     * Le conteneur des menus.
     */
    protected JPanel cards, menuPanel;

    /**
     * Les boutons du menu principal.
     */
    private JButton boutonCreer, boutonCharger;

    /**
     * La configuration de l'application.
     */
    private Configuration configuration;

    /**
     * Le menu de configuration.
     */
    private ConfigurationMenu configurationMenu;

    /**
     * Construire un menu principal pour l'application.
     */
    public RogueMenu() {

        this.setTitle("Simulation 2D");
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);

        cardLayout = new CardLayout();
        cards = new JPanel(cardLayout);

        Dimension tailleBouton = new Dimension(200, 200);
        Font policeBouton = new Font("Arial", Font.PLAIN, 40);

        boutonCreer = new JButton("Creer une configuration");
        boutonCreer.setFont(policeBouton);
        boutonCreer.setPreferredSize(tailleBouton);

        boutonCharger = new JButton("Charger une configuration");
        boutonCharger.setFont(policeBouton);
        boutonCharger.setPreferredSize(tailleBouton);

        this.menuPanel = new JPanel();
        this.menuPanel.setLayout(new GridLayout(2,1,10,30));
        this.menuPanel.add(boutonCreer);
        // menuPanel.add(Box.createVerticalStrut(10));
        this.menuPanel.add(boutonCharger);
        cards.add(this.menuPanel, "Menu");

        boutonCreer.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                RogueMenu.this.configuration = new Configuration();
                RogueMenu.this.launchConfigurationMenu();
            }
        });

        boutonCharger.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileChooser = new JFileChooser("./configs/");
                int returnValue = fileChooser.showOpenDialog(RogueMenu.this);

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    File selectedFile = fileChooser.getSelectedFile();
                    RogueMenu.this.configuration = Configuration.charger(selectedFile);
                    RogueMenu.this.launchConfigurationMenu();
                }
            }
        });

        this.getContentPane().add(cards);
        this.setSize(800, 800);
        this.setLocationRelativeTo(null);
        this.setVisible(true);
    }

    /**
     * Lancer le menu de configuration.
     */
    public void launchConfigurationMenu() {
        configurationMenu = new ConfigurationMenu(configuration, this);
        cards.add(configurationMenu, "Configuration");
        cardLayout.show(cards, "Configuration");
        this.pack();
    }

}
