
public class CorrerVisitor implements Visitor{

	public void visit(Dog animal) {
		animal.correr("O cahorro está correndo");
	}

	public void visit(Cat animal) {
		animal.correr("O gato está correndo");
	}
}
