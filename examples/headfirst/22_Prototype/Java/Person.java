//Este fonte foi retirado de: http://www.avajava.com/tutorials/lessons/prototype-pattern.html
//Ultimo acesso em Agosto de 2013

public class Person implements Prototype {

	String name;

	public Person(String name) {
		this.name = name;
	}

	public Prototype doClone() {
		return new Person(name);
	}

	public String toString() {
		return "This person is named " + name;
	}
}
