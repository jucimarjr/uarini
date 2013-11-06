//Este fonte foi retirado de: http://javapapers.com/design-patterns/bridge-design-pattern/
//Ultimo acesso em Agosto de 2013

public class Car extends Vehicle {
	 
	  public Car(Workshop workShop1, Workshop workShop2) {
	    super(workShop1, workShop2);
	  }
	 
	  public void manufacture() {
	    System.out.print("Car ");
	    workShop1.work();
	    workShop2.work();
	 
	  }
	 
}