public class Pet extends Animal{
	
	Animal Animal;
	
	public Pet(Animal animal) {
		super(animal.getNome(), animal.getIdade());
		this.Animal = animal;
	}
	
	public void falar(){
		Animal.falar();
	}
}
