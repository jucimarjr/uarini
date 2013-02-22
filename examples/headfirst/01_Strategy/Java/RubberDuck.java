public class RubberDuck extends Duck {
	public RubberDuck(){
		quackBehaviour = new Squeak();
		flyBehaviour = new FlyNoWay();
	}
	
	public void display(){
		System.out.println("I'm just a small Rubber Duck...");
	}
}
