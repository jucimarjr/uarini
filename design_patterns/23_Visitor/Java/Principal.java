
public class Principal {

	public static void main(String[] args) {
		
		Visitor fala = new FalarVisitor();
		Visitor corre = new CorrerVisitor();
		
		Animal dog = new Dog();
		Animal cat = new Cat();
		
		dog.accept(fala);
		cat.accept(fala);
		
		dog.accept(corre);
		cat.accept(corre);
	}
}
