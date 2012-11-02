public abstract class Animal {
	private String Nome;
	private int Idade;
	
	public Animal(String nome, int idade) {
		this.Nome = nome;
		this.Idade = idade;
	}
	
	public abstract void falar();
	
	public String getNome() {
		return Nome;
	}
	
	public int getIdade() {
		return Idade;
	}
	
	public void setNome(String nome) {
		Nome = nome;
	}
	
	public void setIdade(int idade) {
		Idade = idade;
	}
}
