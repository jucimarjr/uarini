
public class FalarVisitor implements Visitor {

	public void visit(Dog animal) {
		animal.falar("O cahorro está latindo");
	}

	public void visit(Cat animal) {
		animal.falar("O gato está miando");
	}
}
