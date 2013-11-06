//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Visitor
//Ultimo acesso em Agosto de 2013

public class Car implements CarElement {
	CarElement[] elements;

	public Car() {
		this.elements = new CarElement[] { new Wheel("front left"),
				new Wheel("front right"), new Wheel("back left"),
				new Wheel("back right"), new Body(), new Engine() };
	}

	public void accept(CarElementVisitor visitor) {
		for (CarElement elem : elements) {
			elem.accept(visitor);
		}
		visitor.visit(this);
	}
}
