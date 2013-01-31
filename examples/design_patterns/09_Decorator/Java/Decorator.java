
public class Decorator {
	
	public static void main(String[] args) {
		
		Animal dog = new Dog("Rex", 7);
		
		Animal cat = new Cat("Lili", 5);
		
		Animal pet1 = new Pet(dog);
		Animal pet2 = new Pet(cat);
		
		pet1.falar();
		pet2.falar();
	}
}
