//Este fonte foi retirado de: http://www.fluffycat.com/Java-Design-Patterns/Flyweight/
//Ultimo acesso em Agosto de 2013

class TestFlyweight {

	public static void main(String[] args) {
	
		TeaRestroom teaRestroom = new TeaRestroom();
		teaRestroom.teaFlavorFactory = new TeaFlavorFactory();

		teaRestroom.takeOrders("chai", 2);
		teaRestroom.takeOrders("chai", 2);
		teaRestroom.takeOrders("camomile", 1);
		teaRestroom.takeOrders("camomile", 1);
		teaRestroom.takeOrders("earl grey", 1);
		teaRestroom.takeOrders("camomile", 897);
		teaRestroom.takeOrders("chai", 97);
		teaRestroom.takeOrders("chai", 97);
		teaRestroom.takeOrders("camomile", 3);
		teaRestroom.takeOrders("earl grey", 3);
		teaRestroom.takeOrders("chai", 3);
		teaRestroom.takeOrders("earl grey", 96);
		teaRestroom.takeOrders("camomile", 552);
		teaRestroom.takeOrders("chai", 121);
		teaRestroom.takeOrders("earl grey", 121);

		for (int i = 0; i < teaRestroom.ordersMade; i++) {
			teaRestroom.flavors[i].serveTea(teaRestroom.tables[i]);
		}
		System.out.println(" ");

		teaRestroom.showTotalTeaFlavorsMade();
	}
}
