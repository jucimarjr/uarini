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