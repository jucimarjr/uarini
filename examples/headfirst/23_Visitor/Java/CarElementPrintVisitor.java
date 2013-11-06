//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Visitor
//Ultimo acesso em Agosto de 2013

public class CarElementPrintVisitor implements CarElementVisitor {
	public void visit(Wheel wheel) {
		System.out.println("Visiting " + wheel.getName() + " wheel");
	}

	public void visit(Engine engine) {
		System.out.println("Visiting engine");
	}

	public void visit(Body body) {
		System.out.println("Visiting body");
	}

	public void visit(Car car) {
		System.out.println("Visiting car");
	}
}
