//Este fonte foi retirado de: http://www.avajava.com/tutorials/lessons/prototype-pattern.html
//Ultimo acesso em Agosto de 2013

public class Dog implements Prototype {

	String sound;

	public Dog(String sound) {
		this.sound = sound;
	}

	public Prototype doClone() {
		return new Dog(sound);
	}

	public String toString() {
		return "This dog says " + sound;
	}
}
