//Este fonte foi retirado de: http://javapapers.com/design-patterns/bridge-design-pattern/
//Ultimo acesso em Agosto de 2013

public class Bike extends Vehicle {
	 
	  public Bike(Workshop workShop1, Workshop workShop2) {
	    super(workShop1, workShop2);
	  }
	 
	  public void manufacture() {
	    System.out.print("Bike ");
	    workShop1.work();
	    workShop2.work();
	  }
	 
}
