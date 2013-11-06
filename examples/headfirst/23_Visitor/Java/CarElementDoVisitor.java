//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Visitor
//Ultimo acesso em Agosto de 2013

public class CarElementDoVisitor implements CarElementVisitor {
	public void visit(Wheel wheel) {
		System.out.println("Kicking my " + wheel.getName() + " wheel");
	}

	public void visit(Engine engine) {
		System.out.println("Starting my engine");
	}

	public void visit(Body body) {
		System.out.println("Moving my body");
	}

	public void visit(Car car) {
		System.out.println("Starting my car");
	}
}
