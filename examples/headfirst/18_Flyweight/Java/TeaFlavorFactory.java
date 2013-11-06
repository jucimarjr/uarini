//Este fonte foi retirado de: http://www.fluffycat.com/Java-Design-Patterns/Flyweight/
//Ultimo acesso em Agosto de 2013

public class TeaFlavorFactory {
	TeaFlavor[] flavors = new TeaFlavor[10];
	
	int teasMade = 0;

	public TeaFlavor getTeaFlavor(String flavorToGet) {
		if (teasMade > 0) {
			for (int i = 0; i < teasMade; i++) {
				if (flavorToGet.equals((flavors[i]).getFlavor())) {
					return flavors[i];
				}
			}
		}
		flavors[teasMade] = new TeaFlavor(flavorToGet);
		return flavors[teasMade++];
	}

	public int getTotalTeaFlavorsMade() {
		return teasMade;
	}
}
