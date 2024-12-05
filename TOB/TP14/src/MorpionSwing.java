import javax.swing.*;
import java.awt.*;
import javax.swing.event.*;
import java.awt.event.*;
import java.util.*;

/**
 * Programmation d'un jeu de Morpion avec une interface graphique Swing.
 *
 * REMARQUE : Dans cette solution, le patron MVC n'a pas été appliqué !
 * On a un modèle (?), une vue et un contrôleur qui sont fortement liés.
 *
 * @author Xavier Crégut
 * @version $Revision: 1.4 $
 */

public class MorpionSwing {

	// les images à utiliser en fonction de l'état du jeu.
	private static final Map<ModeleMorpion.Etat, ImageIcon> images = new HashMap<ModeleMorpion.Etat, ImageIcon>();
	static {
		images.put(ModeleMorpion.Etat.VIDE, new ImageIcon("../assets/blanc.jpg"));
		images.put(ModeleMorpion.Etat.CROIX, new ImageIcon("../assets/croix.jpg"));
		images.put(ModeleMorpion.Etat.ROND, new ImageIcon("../assets/rond.jpg"));
	}

	// Choix de réalisation :
	// ----------------------
	//
	// Les attributs correspondant à la structure fixe de l'IHM sont définis
	// « final static » pour montrer que leur valeur ne pourra pas changer au
	// cours de l'exécution. Ils sont donc initialisés sans attendre
	// l'exécution du constructeur !

	private ModeleMorpion modele; // le modèle du jeu de Morpion

	// Les éléments de la vue (IHM)
	// ----------------------------

	/** Fenêtre principale */
	private JFrame fenetre;

	/** Bouton pour quitter */
	private final JButton boutonQuitter = new JButton("Q");

	/** Bouton pour commencer une nouvelle partie */
	private final JButton boutonNouvellePartie = new JButton("N");

	/** Cases du jeu */
	private final JLabel[][] cases = new JLabel[3][3];

	/** Zone qui indique le joueur qui doit jouer */
	private final JLabel joueur = new JLabel();

	// Le constructeur
	// ---------------

	/** Construire le jeu de morpion */
	public MorpionSwing() {
		this(new ModeleMorpionSimple());
	}

	/** Construire le jeu de morpion */
	public MorpionSwing(ModeleMorpion modele) {
		// Initialiser le modèle
		this.modele = modele;

		// Créer les cases du Morpion
		for (int i = 0; i < this.cases.length; i++) {
			for (int j = 0; j < this.cases[i].length; j++) {
				this.cases[i][j] = new JLabel();
			}
		}

		// Initialiser le jeu
		this.recommencer();

		// Construire la vue (présentation)
		// Définir la fenêtre principale
		this.fenetre = new JFrame("Morpion");
		this.fenetre.setLocation(200, 200);

		// Construire le contrôleur (gestion des événements)
		this.fenetre.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		java.awt.Container contenu = this.fenetre.getContentPane();

		contenu.setLayout(new GridLayout(4, 3));

		for (int i = 0; i < this.cases.length; i++) {
			for (int j = 0; j < this.cases.length; j++) {
				JLabel case_ = this.cases[i][j];
				case_.setIcon(images.get(ModeleMorpion.Etat.VIDE));
				case_.setHorizontalAlignment(JLabel.CENTER);
				case_.setVerticalAlignment(JLabel.CENTER);
				case_.addMouseListener(new ActionJouer(modele, i, j));
				contenu.add(case_);
			}
		}

		boutonNouvellePartie.addActionListener(new ActionNouvellePartie(this));
		contenu.add(boutonNouvellePartie);

		// this.joueur.setIcon(images.get(ModeleMorpion.Etat.ROND));
		this.joueur.setHorizontalAlignment(JLabel.CENTER);
		this.joueur.setVerticalAlignment(JLabel.CENTER);
		contenu.add(this.joueur);

		boutonQuitter.addActionListener(new ActionQuitter(this.fenetre));
		contenu.add(boutonQuitter);

		// afficher la fenêtre
		this.fenetre.pack(); // redimensionner la fenêtre
		this.fenetre.setVisible(true); // l'afficher
		this.fenetre.setJMenuBar(new MorpionMenuBar(this, this.fenetre));

	}

	// Quelques réactions aux interactions de l'utilisateur
	// ----------------------------------------------------

	/** Recommencer une nouvelle partie. */
	public void recommencer() {
		this.modele.recommencer();

		// Vider les cases
		for (int i = 0; i < this.cases.length; i++) {
			for (int j = 0; j < this.cases[i].length; j++) {
				this.cases[i][j].setIcon(images.get(this.modele.getValeur(i, j)));
			}
		}

		// Mettre à jour le joueur
		joueur.setIcon(images.get(modele.getJoueur()));
	}

	// La méthode principale
	// ---------------------
	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				new MorpionSwing();
			}
		});
	}

}

class ActionJouer extends MouseAdapter {

	private ModeleMorpion morpion;
	private int x, y;

	ActionJouer(ModeleMorpion morpion, int x, int y) {
		super();
		this.morpion = morpion;
		this.x = x;
		this.y = y;
	}

	@Override
	public void mouseClicked(MouseEvent ev) {
		try {
			this.morpion.cocher(this.x, this.y);
		} catch (CaseOccupeeException e) {
			// TODO ?
		}
	}
}


class ActionNouvellePartie implements ActionListener {

	private MorpionSwing morpion;

	ActionNouvellePartie(MorpionSwing morpion) {
		super();
		this.morpion = morpion;
	}

	public void actionPerformed(ActionEvent e) {
		this.morpion.recommencer();
	}
}

class ActionQuitter implements ActionListener {

	private JFrame fenetre;

	ActionQuitter(JFrame fenetre) {
		super();
		this.fenetre = fenetre;
	}

	public void actionPerformed(ActionEvent e) {
		this.fenetre.dispose();
	}
}

class MorpionMenuBar extends JMenuBar {

	private JMenuItem nouvellePartie, quitter;

	MorpionMenuBar(MorpionSwing morpion, JFrame fenetre) {
		super();
		JMenu menu = new JMenu("Jeu");
		this.add(menu);

		// option nouvelle partie
		nouvellePartie = new JMenuItem("Nouvelle partie");
		nouvellePartie.addActionListener(new ActionNouvellePartie(morpion));
		menu.add(nouvellePartie);

		// option quitter
		quitter = new JMenuItem("Quitter");
		quitter.addActionListener(new ActionQuitter(fenetre));
		menu.add(quitter);
	}
}