//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Visitor
//Ultimo acesso em Agosto de 2013

public class Body implements CarElement {
	public void accept(CarElementVisitor visitor) {
		visitor.visit(this);
	}
}
