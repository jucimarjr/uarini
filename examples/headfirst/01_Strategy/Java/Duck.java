//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public abstract class Duck {
	FlyBehaviour flyBehaviour;
	QuackBehaviour quackBehaviour;
	
	public abstract void display();
	
	public void performFly(){
		flyBehaviour.fly();
	}
	
	public void performQuack(){
		quackBehaviour.quack();
	}
	
	public void swim(){
		System.out.println("All ducks float, even decoys!");
	}
}
