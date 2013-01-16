public class Main {
	public static void main(String[] args) {
		Transporte transp = new Transporte();
		Aviao aviao = new Aviao();
		Navio navio = new Navio();
		Onibus onibus = new Onibus();
		System.out.println("Polimorfismo (Sobreposicao)");
		
		transp.exibeDados();
		aviao.exibeDados();
		navio.exibeDados();
		onibus.exibeDados();
		
	}
}