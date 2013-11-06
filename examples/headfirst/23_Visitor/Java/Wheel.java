//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Visitor
//Ultimo acesso em Agosto de 2013

public class Wheel implements CarElement {
	private String name;

	public Wheel(String name) {
		this.name = name;
	}

	public String getName() {
		return this.name;
	}

	public void accept(CarElementVisitor visitor) {
		visitor.visit(this);
	}
}
