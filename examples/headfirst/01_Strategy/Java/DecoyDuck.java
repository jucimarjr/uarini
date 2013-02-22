public class DecoyDuck extends Duck {
	public DecoyDuck(){
		quackBehaviour = new MuteQuack();
		flyBehaviour = new FlyNoWay();
	}
	
	public void display(){
		System.out.println("I'm only a wood carved Decoy Duck...");
	}
}
