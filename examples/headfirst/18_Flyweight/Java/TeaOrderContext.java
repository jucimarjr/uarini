//Este fonte foi retirado de: http://www.fluffycat.com/Java-Design-Patterns/Flyweight/
//Ultimo acesso em Agosto de 2013

public class TeaOrderContext {
	int tableNumber;

	TeaOrderContext(int tableNumber) {
		this.tableNumber = tableNumber;
	}

	public int getTable() {
		return this.tableNumber;
	}
}
