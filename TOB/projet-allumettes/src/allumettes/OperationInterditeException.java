package allumettes;

/**
 * Exception qui indique qu'une opération interdite est tentée.
 *
 * @author Vianney Hervy
 */
public class OperationInterditeException extends RuntimeException {

    private int coup;

    /**
     * Construire une exception indiquant qu'une opération interdite est tentée.
     *
     * @param coup le coup interdit joué
     */
    public OperationInterditeException(int coup) {
        super();
        this.coup = coup;
    }

    public int getCoup() {
        return this.coup;
    };
}
