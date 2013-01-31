
public class Chamar {
	
	private Cachorro cachorro;
	private Gato gato;
	
	public Chamar() {
		this.cachorro = new Cachorro();
		this.gato = new Gato();
	}
	
	public void chamar(){
		cachorro.falar();
		gato.falar();		
	}
}
