//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Visitor
//Ultimo acesso em Agosto de 2013

public interface CarElementVisitor {
	void visit(Wheel wheel);

	void visit(Engine engine);

	void visit(Body body);

	void visit(Car car);
}
