package simulation2D.menu;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.awt.Color;

import simulation2D.objects.Particule;
import simulation2D.physics.Vecteur2D;
import simulation2D.Launch;
import simulation2D.objects.Famille;
import simulation2D.objects.DicoRelations2D;

/**
 * Gère le menu textuel de la première itération (sera remplacé par un menu
 * graphique plus tard).
 *
 * @author Baptiste Gomez
 */
public class MenuTextuel {

    /**
     * Les particules de la simulation.
     */
    private List<Particule> particules;

    /**
     * Les familles de particules de la simulation.
     */
    private List<Famille> familles;

    /**
     * Les relations entre les familles de particules.
     */
    private DicoRelations2D relations;

    /**
     * Créer un menu textuel pour la simulation.
     */
    public MenuTextuel() {
        this.particules = new ArrayList<>();
        this.familles = new ArrayList<>();
        this.relations = new DicoRelations2D();
    }

    /**
     * Obtenir les particules de la simulation.
     *
     * @return les particules de la simulation.
     */
    public List<Particule> getParticules() {
        return this.particules;
    }

    /**
     * Obtenir les familles de particules de la simulation.
     *
     * @return les familles de particules de la simulation.
     */
    public List<Famille> getFamilles() {
        return this.familles;
    }

    /**
     * Obtenir les relations entre les familles de particules.
     *
     * @return les relations entre les familles de particules.
     */
    public DicoRelations2D getRelations() {
        return this.relations;
    }

    /**
     * Exécuter le menu textuel.
     */
    public void execMenu() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("Combien de familles voulez-vous ? ");
        int nbFamilles = scanner.nextInt();
        System.out.print("Combien de particules par famille voulez-vous ? ");
        int nbParticules = scanner.nextInt();
        scanner.close();

        genererSimulation(nbParticules, nbFamilles);

        genererRelations();
    }

    /**
     * Générer une simulation aléatoire.
     *
     * @param nbParticules le nombre de particules par famille
     * @param nbFamilles   le nombre de familles
     */
    private void genererSimulation(int nbParticules, int nbFamilles) {
        Famille famille;
        for (int i = 0; i < nbFamilles; i++) {
            famille = genererFamille();
            this.familles.add(famille);
            Particule particule;
            for (int j = 0; j < nbParticules; j++) {
                particule = genererParticule(famille);
                this.particules.add(particule);
            }
        }
    }

    /**
     * Générer une particule aléatoire.
     *
     * @param famille la famille de la particule
     * @return la particule générée
     */
    private Particule genererParticule(Famille famille) {
        Random rand = new Random();
        Particule res = new Particule(famille, new Vecteur2D(rand.nextInt(Launch.WIDTH), rand.nextInt(Launch.HEIGHT)),
                new Vecteur2D(rand.nextInt(100) - 50, rand.nextInt(100) - 50).times(2));
        return res;
    }

    /**
     * Générer une famille aléatoire.
     *
     * @return la famille générée
     */
    private Famille genererFamille() {
        Random rand = new Random();
        Color[] colors = {
                Color.BLACK, Color.BLUE, Color.CYAN, Color.DARK_GRAY, Color.GRAY,
                Color.GREEN, Color.MAGENTA, Color.ORANGE, Color.PINK,
                Color.RED, Color.YELLOW, Color.WHITE
        };
        Famille res = new Famille("", colors[rand.nextInt(colors.length)], rand.nextInt(100), rand.nextInt(50), 0);
        return res;
    }

    /**
     * Générer des relations aléatoires entre les familles de particules.
     */
    private void genererRelations() {
        Random rand = new Random();
        for (Famille familleAttractice : this.familles) {
            for (Famille familleSubis : this.familles) {
                this.relations.setRelation(familleSubis, familleAttractice, rand.nextInt(100));
            }
        }
    }

}
