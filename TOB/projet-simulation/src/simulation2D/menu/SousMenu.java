package simulation2D.menu;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import java.util.ArrayList;
import java.util.List;
import simulation2D.objects.Famille;
import simulation2D.objects.Particule;
import simulation2D.objects.DicoRelations2D;
import simulation2D.physics.Vecteur2D;

public class SousMenu extends JDialog {
    private List<Particule> objects;
    private DicoRelations2D relations;
    private Famille famille;
    private JButton boutonOk;

    private Case caseNom;
    private Case caseNombre;
    private Case caseMasse;
    private Case caseVitesse;
    private Case caseRayon;
    private Case caseCouleur;

    // Constructeur Sousmenu
    public SousMenu(JFrame parent, String titre, boolean modal, Famille famille) {
        super(parent, titre, modal);
        this.setSize(850, 600);
        this.setLocationRelativeTo(null);
        this.setResizable(false);
        this.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);

        // Initialisation de la liste des familles et des objets
        this.objects = new ArrayList<>();
        this.relations = new DicoRelations2D();
        this.famille = famille;
        this.majFamille();
        setVisible(true);
    }

    // La méthode crée les cases à compléter
    public void majFamille() {

        // Ajout des différentes cases au sous-menu
        JPanel fenetre = new JPanel();
        fenetre.setBackground(Color.white);

        // Création des différentes cases
        this.caseNom = new Case("Nom de la famille", "Saisir un nom :  ", this.famille.getNom());
        this.caseNombre = new Case("Nombre de particules", "Saisir un nombre : ",
                String.valueOf(this.famille.getNbParticules()));
        this.caseMasse = new Case("Masse", "Saisir une masse : ", String.valueOf(this.famille.getMass()));
        this.caseVitesse = new Case("Vitesse", "Vx :", "Vy :", "Vitesse par défaut", this.famille.getVitesseInit());
        this.caseRayon = new Case("Rayon", "Saisir un rayon : ", String.valueOf(this.famille.getRadius()));
        this.caseCouleur = new Case("Couleur", this.famille.getColor());

        // Ajout des cases au contenu
        fenetre.add(this.caseNom);
        fenetre.add(this.caseNombre);
        fenetre.add(this.caseMasse);
        fenetre.add(this.caseRayon);
        fenetre.add(this.caseVitesse);
        fenetre.add(this.caseCouleur);

        JPanel control = new JPanel();
        boutonOk = new JButton("OK");
        boutonOk.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                String nomFamille = SousMenu.this.caseNom.getTextsousFenetre();
                int nombreParticules = Integer.parseInt(SousMenu.this.caseNombre.getTextsousFenetre());
                double masseFamille = Double.parseDouble(SousMenu.this.caseMasse.getTextsousFenetre());
                Color couleurFamille = SousMenu.this.caseCouleur.getSelectedColor();
                boolean defaut = SousMenu.this.caseVitesse.getDefaut();

                double vX;
                double vY;
                if (!defaut) {
                    vX = Double.parseDouble(SousMenu.this.caseVitesse.getVitesse().get(0));
                    vY = Double.parseDouble(SousMenu.this.caseVitesse.getVitesse().get(1));
                } else {
                    vX = 0;
                    vY = 0;
                }

                double rayonFamille = Double.parseDouble(SousMenu.this.caseRayon.getTextsousFenetre());

                genererSimulation(nomFamille, nombreParticules, rayonFamille, masseFamille, couleurFamille, 1, defaut,
                        vX, vY);
                setVisible(false);
            }
        });

        JButton boutonAnnuler = new JButton("Annuler");
        boutonAnnuler.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent arg0) {
                setVisible(false);
            }
        });

        control.add(boutonOk);
        control.add(boutonAnnuler);
        this.getContentPane().add(fenetre, BorderLayout.CENTER);
        this.getContentPane().add(control, BorderLayout.SOUTH);
    }

    private void genererSimulation(String nom, int nombre, Double rayon, Double masse, Color couleur, int nbFamilles,
            boolean defaut, Double vX, Double vY) {
        this.famille.setColor(couleur);
        this.famille.setMass(masse);
        this.famille.setNbParticules(nombre);
        this.famille.setNom(nom);
        this.famille.setRadius(rayon);
        this.famille.setVitesseInit(new Vecteur2D(vX, vY));
    }

    public Famille getFamille() {
        return this.famille;
    }

    public List<Particule> getParticules() {
        return this.objects;
    }

    public DicoRelations2D getRelations() {
        return this.relations;
    }
}
