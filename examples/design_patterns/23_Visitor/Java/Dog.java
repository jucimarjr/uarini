
public class Dog extends Animal {

	public void falar(String text) {
		System.out.println(text);
	}
	
	public void correr(String text) {
		System.out.println(text);		
	}

	public void accept(Visitor visitor) {
		visitor.visit(this);
	}
}
