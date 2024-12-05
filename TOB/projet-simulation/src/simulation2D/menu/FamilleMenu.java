package simulation2D.menu;

import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JTextField;
import javax.swing.JFormattedTextField;
import javax.swing.SwingConstants;
import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;

import java.util.ArrayList;
import java.util.List;

import java.awt.FlowLayout;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.text.NumberFormat;

import simulation2D.objects.Famille;
import simulation2D.simulation.Configuration;

public class FamilleMenu extends JPanel {

    /**
     * La famille concernée.
     */
    private Famille famille;

    /**
     * La configuration concernée.
     */
    private Configuration config;

    /**
     * Le champ de texte pour le nom de la famille.
     */
    private JLabel nameField;

    /**
     * Les bouton pour afficher/accéder à la modification des différents champs de
     * la famille.
     */
    private JButton colorButton, modifButton, relationsButton, supprButton;

    /**
     * Le champ de texte pour le rayon de la famille.
     */
    private JFormattedTextField radiusField;

    /**
     * Le champ de texte pour la masse de la famille.
     */
    private JFormattedTextField massField;

    /**
     * Le formatteur de nombre.
     */
    private final NumberFormat numberFormat = NumberFormat.getNumberInstance();

    private List<JTextField> relationValues;
    private List<Famille> familles;
    private JFrame main;

    /**
     * Créer un menu pour une famille.
     *
     * @param famille la famille concernée
     * @param config  la configuration de la simulation
     */
    public FamilleMenu(Famille famille, Configuration config) {
        this.famille = famille;
        this.config = config;

        this.setLayout(new FlowLayout());

        nameField = new JLabel(this.famille.getNom());
        nameField.setHorizontalAlignment(SwingConstants.RIGHT);

        this.add(nameField);

        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(60, 30));
        colorButton.setBackground(this.famille.getColor());
        this.add(colorButton);

        modifButton = new JButton("Modifier");
        modifButton.setPreferredSize(new Dimension(80, 30));
        modifButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                SousMenu menuFamille = new SousMenu(null, "Modifier une famille", true, FamilleMenu.this.famille);
                menuFamille.majFamille();
                FamilleMenu.this.famille = menuFamille.getFamille();
                FamilleMenu.this.colorButton.setBackground(FamilleMenu.this.famille.getColor());
                FamilleMenu.this.nameField.setText(FamilleMenu.this.famille.getNom());
            }
        });
        this.add(modifButton);

        relationsButton = new JButton("Relations");
        relationsButton.setPreferredSize(new Dimension(100, 30));
        relationsButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                JPanel content = new JPanel();
                content.setBackground(Color.white);
                content.setSize(550, 550);
                // Nom
                JTextField nomFamille = new JTextField(FamilleMenu.this.famille.getNom());
                content.add(nomFamille);
                FamilleMenu.this.main = new JFrame("Relations sur " + FamilleMenu.this.famille.getNom());
                FamilleMenu.this.main.setSize(550, 550);
                FamilleMenu.this.main.setResizable(false);
                FamilleMenu.this.familles = FamilleMenu.this.config.getFamilles();
                FamilleMenu.this.relationValues = new ArrayList<>();
                for (Famille famille : FamilleMenu.this.familles) {
                    JTextField nombre;
                    JLabel nombreLabel;
                    JPanel panNombre = new JPanel();
                    panNombre.setBackground(Color.white);
                    panNombre.setPreferredSize(new Dimension(250, 100));
                    nombre = new JTextField(String.valueOf(
                            FamilleMenu.this.config.getRelations().getIntensite(FamilleMenu.this.famille, famille)));
                    nombre.setPreferredSize(new Dimension(120, 35));
                    panNombre.setBorder(BorderFactory.createTitledBorder(famille.getNom()));
                    nombreLabel = new JLabel("Saisir relation (nombre) :");
                    panNombre.add(nombreLabel);
                    panNombre.add(nombre);
                    content.add(panNombre);
                    FamilleMenu.this.relationValues.add(nombre);
                }
                JPanel control = new JPanel();
                JButton boutonOk = new JButton("Ok");
                boutonOk.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent arg0) {
                        for (int i = 0; i < FamilleMenu.this.relationValues.size(); i++) {
                            FamilleMenu.this.config.getRelations().setRelation(FamilleMenu.this.famille,
                                    FamilleMenu.this.familles.get(i),
                                    Double.parseDouble(relationValues.get(i).getText()));
                        }
                        FamilleMenu.this.main.dispose();
                    }
                });
                control.add(boutonOk);
                main.getContentPane().add(content, BorderLayout.CENTER);
                main.getContentPane().add(control, BorderLayout.SOUTH);

                main.setVisible(true);
            }
        });
        this.add(relationsButton);

        supprButton = new JButton("Supprimer");
        supprButton.setPreferredSize(new Dimension(100, 30));
        supprButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                FamilleMenu.this.config.supprimerFamille(FamilleMenu.this.famille);
                FamilleMenu.this.setVisible(false);
            }
        });
        this.add(supprButton);
    }
}
