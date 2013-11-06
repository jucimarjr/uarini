//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class RubberDuck extends Duck {
	public RubberDuck(){
		quackBehaviour = new Squeak();
		flyBehaviour = new FlyNoWay();
	}
	
	public void display(){
		System.out.println("I'm just a small Rubber Duck...");
	}
}
