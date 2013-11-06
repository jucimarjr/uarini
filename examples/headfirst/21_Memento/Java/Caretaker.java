//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/memento/java/1
//Ultimo acesso em Agosto de 2013

public class Caretaker {
	private ArrayList<Memento> savedStates = new ArrayList<Memento>();

	public void addMemento(Memento m) {
		savedStates.add(m);
	}

	public Memento getMemento(int index) {
		return savedStates.get(index);
	}
}
