public class TeaOrderContext {
	int tableNumber;

	TeaOrderContext(int tableNumber) {
		this.tableNumber = tableNumber;
	}

	public int getTable() {
		return this.tableNumber;
	}
}
