public class Pet extends Animal{
	
	Animal Animal;
	
	public Pet(Animal animal) {
		super(animal.getNome(), animal.getIdade());
		this.Animal = animal;
	}
	
	public void falar(){
		Animal.falar();
	}

	public String getNome() {
		return Animal.getNome();
	}
	
	public int getIdade() {
		return Animal.getIdade();
	}
	
	public void setNome(String nome) {
		Animal.setNome(nome);
	}
	
	public void setIdade(int idade) {
		Animal.setIdade(idade);
	}
}
