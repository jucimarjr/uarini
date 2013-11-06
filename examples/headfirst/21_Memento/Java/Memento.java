//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/memento/java/1
//Ultimo acesso em Agosto de 2013

public class Memento {
	private String state;

	public Memento(String stateToSave) {
		state = stateToSave;
	}

	public String getSavedState() {
		return state;
	}
}
