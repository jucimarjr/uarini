//Este fonte foi retirado de: http://javapapers.com/design-patterns/bridge-design-pattern/
//Ultimo acesso em Agosto de 2013

public abstract class Vehicle {
	  protected Workshop workShop1;
	  protected Workshop workShop2;
	 
	  protected Vehicle(Workshop workShop1, Workshop workShop2) {
	    this.workShop1 = workShop1;
	    this.workShop2 = workShop2;
	  }
	  abstract public void manufacture();
}