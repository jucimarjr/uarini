//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class DecoyDuck extends Duck {
	public DecoyDuck(){
		quackBehaviour = new MuteQuack();
		flyBehaviour = new FlyNoWay();
	}
	
	public void display(){
		System.out.println("I'm only a wood carved Decoy Duck...");
	}
}
