//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class RedheadDuck extends Duck {
	public RedheadDuck(){
		quackBehaviour = new Quack();
		flyBehaviour = new FlyWithWings();
	}
	
	public void display(){
		System.out.println("I'm a real Red-Head Duck!");
	}
}
