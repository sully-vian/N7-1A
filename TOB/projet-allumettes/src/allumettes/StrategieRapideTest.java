package allumettes;

import org.junit.*;
import static org.junit.Assert.*;

public class StrategieRapideTest {

    private StrategieRapide strategieRapide;
    private JeuReel jeu13;

    @Before
    public void setUp() {
        strategieRapide = new StrategieRapide();
        jeu13 = new JeuReel(13);
    }

    @Test
    public void getPriseTest() throws CoupInvalideException {
        int prise13;

        for (int i = 0; i < 4; i++) {
            prise13 = strategieRapide.getPrise(jeu13);
            assertEquals("Il reste " + jeu13.getNombreAllumettes() + " (>= 3) allumettes dans le jeu", 3, prise13);
            jeu13.retirer(3);
        }

        prise13 = strategieRapide.getPrise(jeu13);
        assertEquals("Il reste " + jeu13.getNombreAllumettes() + " allumette dans le jeu", 1, prise13);
    }
}
