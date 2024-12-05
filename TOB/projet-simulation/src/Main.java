import simulation2D.menu.RogueMenu;

/**
 * La classe <code>Main</code> est la classe principale du projet. Elle ne fait
 * pas partie du package simulation2D.
 *
 * @author Vianney Hervy
 */
public class Main {

    /**
     * Lancer l'application.
     *
     * @param args les arguments de la ligne de commande ( ne sont pas utilisés)
     */
    public static void main(String[] args) {
        //Launch.launch();
        new RogueMenu();
    }

    /**
     * Constructeur privé pour empêcher l'instanciation de la classe.
     */
    private Main() {
    }
}
