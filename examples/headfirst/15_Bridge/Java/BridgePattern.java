//Este fonte foi retirado de: http://javapapers.com/design-patterns/bridge-design-pattern/
//Ultimo acesso em Agosto de 2013

public class BridgePattern {
	 
	  public static void main(String[] args) {
	 
	    Vehicle vehicle1 = new Car(new Produce(), new Assemble());
	    vehicle1.manufacture();
	    Vehicle vehicle2 = new Bike(new Produce(), new Assemble());
	    vehicle2.manufacture();
	  }
}
