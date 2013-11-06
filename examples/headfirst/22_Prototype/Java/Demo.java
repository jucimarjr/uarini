//Este fonte foi retirado de: http://www.avajava.com/tutorials/lessons/prototype-pattern.html
//Ultimo acesso em Agosto de 2013

public class Demo {

	public static void main(String[] args) {

		System.out.println("Creating person 1...");
		Person person1 = new Person("Fred");
		System.out.println("person 1: " + person1);
		
		System.out.println("Cloning person 1 to become person 2...");
		Person person2 = (Person) person1.doClone();
		System.out.println("person 2: " + person2);

		System.out.println("Creating dog 1...");
		Dog dog1 = new Dog("Wooof!");
		System.out.println("dog 1: " + dog1);
		
		System.out.println("Cloning dog 1 to become dog 2...");
		Dog dog2 = (Dog) dog1.doClone();
		System.out.println("dog 2: " + dog2);

	}

}
