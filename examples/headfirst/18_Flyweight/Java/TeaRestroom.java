public class TeaRestroom {
	TeaFlavor[] flavors = new TeaFlavor[100];
	TeaOrderContext[] tables = new TeaOrderContext[100];
	int ordersMade = 0;
	TeaFlavorFactory teaFlavorFactory;

	public void takeOrders(String flavorIn, int table) {
		flavors[ordersMade] = teaFlavorFactory.getTeaFlavor(flavorIn);
		tables[ordersMade++] = new TeaOrderContext(table);
	}
	
	public void showTotalTeaFlavorsMade(){
		System.out.println("Total tea Flavor Objects made: " + teaFlavorFactory.getTotalTeaFlavorsMade());
	}
}
