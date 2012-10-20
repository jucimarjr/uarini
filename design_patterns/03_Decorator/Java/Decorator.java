
public class Decorator {
	
	public static void main(String[] args) {
		
		Animal dog = new Dog("Rex", 7);
		
		Cat cat = new Cat("Lili", 5);
		
		Pet pet1 = new Pet(dog);
		Pet pet2 = new Pet(cat);
		
		pet1.falar();
		pet2.falar();
	}
}
