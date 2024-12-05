package simulation2D.menu;

import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JCheckBox;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.ArrayList;
import javax.swing.JColorChooser;
import java.awt.FlowLayout;
import javax.swing.colorchooser.ColorSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import simulation2D.physics.Vecteur2D;

public class Case extends JPanel {
    private JTextField sousFenetre, vX, vY;
    private JCheckBox caseVitesseDefaut;
    private Color couleurSelectionne;
    private JPanel carreCouleur;
    private JColorChooser palette;

    // Constructeur pour les cases avec zone de texte (rayon, masse, nombre, nom)
    public Case(String bord, String instruction, String defaut) {
        super();
        this.setBackground(Color.white);
        this.setPreferredSize(new Dimension(250, 100));
        sousFenetre = new JTextField(defaut);
        sousFenetre.setPreferredSize(new Dimension(120, 35));
        this.setBorder(BorderFactory.createTitledBorder(bord));
        JLabel nomLabel = new JLabel(instruction);
        this.add(nomLabel);
        this.add(sousFenetre);
    }

    // Méthode pour accéder au contenu du JTextField
    public String getTextsousFenetre() {
        return sousFenetre.getText();
    }

    // Constructeur pour créer une case pour sélectionner une couleur
    public Case(String labelText, Color defaut) {
        super();
        this.setLayout(new FlowLayout());
        this.couleurSelectionne = defaut;

        this.carreCouleur = new JPanel();
        this.carreCouleur.setPreferredSize(new Dimension(50, 50));
        this.carreCouleur.setBackground(couleurSelectionne);

        // Créer une palette de couleur
        this.palette = new JColorChooser();
        this.palette.setPreviewPanel(new JPanel());
        this.palette.setColor(couleurSelectionne);

        ColorSelectionModel model = this.palette.getSelectionModel();
        model.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                Case.this.couleurSelectionne = Case.this.palette.getColor();
                Case.this.carreCouleur.setBackground(couleurSelectionne);
            }
        });

        this.add(new JLabel(labelText));
        this.add(carreCouleur);
        this.add(palette);
    }

    // Constructeur pour la case pour la vitesse
    public Case(String bord, String vx, String vy, String caseParDefaut, Vecteur2D defaut) {
        super();
        this.setBackground(Color.white);
        this.setPreferredSize(new Dimension(500, 100));
        vX = new JTextField(String.valueOf(defaut.getX()));
        vY = new JTextField(String.valueOf(defaut.getY()));
        caseVitesseDefaut = new JCheckBox(caseParDefaut);
        caseVitesseDefaut.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                boolean defaut = caseVitesseDefaut.isSelected();
                // Activer ou désactiver les zones de texte en fonction de l'état de la case à
                // cocher
                vX.setEnabled(!defaut);
                vY.setEnabled(!defaut);
            }
        });
        this.add(caseVitesseDefaut);
        vX.setPreferredSize(new Dimension(50, 35));
        this.add(new JLabel(vx));
        this.add(vX);

        vY.setPreferredSize(new Dimension(50, 35));
        this.add(new JLabel(vy));
        this.add(vY);
        this.setBorder(BorderFactory.createTitledBorder(bord));
    }

    public List<String> getVitesse() {
        List<String> vitesse = new ArrayList<>();
        vitesse.add(vX.getText());
        vitesse.add(vY.getText());
        return vitesse;
    }

    public boolean getDefaut() {
        return caseVitesseDefaut.isSelected();
    }

    public Color getSelectedColor() {
        return couleurSelectionne;
    }
}
