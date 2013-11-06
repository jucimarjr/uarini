//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class MiniDuckSimulator {

	public static void main(String[] args) {
		Duck mallard = new MallardDuck();
		mallard.display();
		mallard.performQuack();
		mallard.performFly();
		mallard.swim();
		
		Duck redhead = new RedheadDuck();
		redhead.display();
		redhead.performQuack();
		redhead.performFly();
		redhead.swim();
		
		Duck rubber = new RubberDuck();
		rubber.display();
		rubber.performQuack();
		rubber.performFly();
		rubber.swim();
		
		Duck decoy = new DecoyDuck();
		decoy.display();
		decoy.performQuack();
		decoy.performFly();
		decoy.swim();
	}
}
