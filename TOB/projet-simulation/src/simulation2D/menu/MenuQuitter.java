package simulation2D.menu;

import simulation2D.simulation.Configuration;

import javax.swing.JFrame;
import javax.swing.JButton;
import javax.swing.JFileChooser;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.File;

/**
 * La classe <code>MenuQuitter</code> est un menu qui s'affiche lorsque
 * l'utilisateur quitte la simulation. Il présente deux boutons : un pour
 * sauvegarder la configuration et un autre pour quitter l'application sans
 * sauvegarder.
 */
public class MenuQuitter extends JFrame {

    /**
     * La configuration de la simulation quittée.
     */
    private Configuration configuration;

    /**
     * La fenêtre de la simulation quittée.
     */
    private JFrame simulationQuittee;

    /**
     * Les boutons du menu quitter.
     */
    private JButton boutonRetour = new JButton("Retourner a la simulation");
    private JButton boutonSauver = new JButton("Sauvegarder la configuration");
    private JButton boutonQuitter = new JButton("Quitter l'application (la configuration sera perdue !)");

    /**
     * Construire un menu quitter pour la simulation quittée.
     *
     * @param configuration     la configuration de la simulation quittée
     * @param simulationQuittee la fenêtre de la simulation quittée
     */
    public MenuQuitter(Configuration configuration, JFrame simulationQuittee) {

        super("Menu Quitter");

        this.configuration = configuration;
        this.simulationQuittee = simulationQuittee;
        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

        this.setLayout(new GridLayout(3, 1));

        this.add(boutonRetour);
        this.add(boutonSauver);
        this.add(boutonQuitter);

        this.pack();
        this.setVisible(true);

        boutonRetour.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MenuQuitter.this.dispose();
            }
        });

        boutonSauver.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                JFileChooser fileChooser = new JFileChooser("./configs/");
                int returnValue = fileChooser.showSaveDialog(MenuQuitter.this);
                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    File file = fileChooser.getSelectedFile();
                    MenuQuitter.this.configuration.sauvegarder(file);
                    boutonQuitter.setText("Quitter l'application");
                }
            }
        });

        boutonQuitter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                MenuQuitter.this.simulationQuittee.dispose();
                MenuQuitter.this.dispose();
                System.exit(0);
            }
        });
    }

}
