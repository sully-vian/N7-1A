package simulation2D.menu;

import simulation2D.simulation.Configuration;
import simulation2D.Launch;
import simulation2D.objects.Famille;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JFileChooser;

import javax.swing.DefaultListModel;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;

import java.awt.event.ActionListener;
import java.io.File;
import java.awt.event.ActionEvent;

public class ConfigurationMenu extends JPanel {

    /**
     * La liste des familles ajoutées.
     */
    private JScrollPane familleMenus;

    /**
     * La configuration en modification.
     */
    private Configuration configuration;

    private JPanel panel;

    private JTextField nameField;

    private JFrame main;

    private RogueMenu mainMenu;

    /**
     * Créer un menu de configuration pour une simulation.
     *
     * @param configuration la configuration à modifier
     */
    public ConfigurationMenu(Configuration configuration, RogueMenu mainMenu) {

        this.configuration = configuration;
        this.panel = new JPanel();
        this.mainMenu = mainMenu;

        this.setLayout(new BorderLayout());

        // Liste des familles
        DefaultListModel<Famille> famillesModel = new DefaultListModel<>();
        for (Famille famille : configuration.getFamilles()) {
            famillesModel.addElement(famille);
        }
        familleMenus = createFamilleMenuList(this.panel);
        this.add(familleMenus, BorderLayout.WEST);

        JPanel menuPanel = new JPanel();
        menuPanel.setLayout(new BoxLayout(menuPanel, BoxLayout.X_AXIS));

        JButton ajoutButton = new JButton("Ajouter");
        ajoutButton.setPreferredSize(new Dimension(80, 30));
        ajoutButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                Famille famille = new Famille("Famille " + String.valueOf(1 + ConfigurationMenu.this.configuration.getRelations().getNbFamilles()));
                SousMenu menuFamille = new SousMenu(null, "Modifier une famille", true, famille);
                menuFamille.majFamille();
                famille = menuFamille.getFamille();
                ConfigurationMenu.this.configuration.ajouterFamille(famille);
                FamilleMenu familleMenu = new FamilleMenu(famille, ConfigurationMenu.this.configuration);
                ConfigurationMenu.this.panel.add(familleMenu);
                update();
                familleMenu.setVisible(true);
            }
        });

        JButton boutonSauver = new JButton("Sauvegarder");
        boutonSauver.setPreferredSize(new Dimension(80, 30));
        boutonSauver.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    JFileChooser fileChooser = new JFileChooser("./configs/");
                    int returnValue = fileChooser.showSaveDialog(ConfigurationMenu.this);
                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        File file = fileChooser.getSelectedFile();
                        ConfigurationMenu.this.configuration.sauvegarder(file);
                    }
                }
        });

        JButton lancerButton = new JButton("Lancer");
        lancerButton.setPreferredSize(new Dimension(80, 30));
        lancerButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                ConfigurationMenu.this.mainMenu.dispose();
                ConfigurationMenu.this.setVisible(false);
                Launch.launch(ConfigurationMenu.this.configuration);
            }
        });

        menuPanel.add(ajoutButton);
        menuPanel.add(boutonSauver);
        menuPanel.add(lancerButton);
        this.add(menuPanel, BorderLayout.SOUTH);
    }

    /**
     * Créer une liste de menus pour les familles de particules.
     *
     * @return la liste des menus des familles
     */
    private JScrollPane createFamilleMenuList(JPanel panel) {

        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        FamilleMenu familleMenu;
        for (Famille famille : configuration.getFamilles()) {
            familleMenu = new FamilleMenu(famille, configuration);
            panel.add(familleMenu);
        }

        return new JScrollPane(panel);
    }

    private void update() {
        this.revalidate();
        this.familleMenus.revalidate();
        this.repaint();
        this.familleMenus.repaint();
    }

}
