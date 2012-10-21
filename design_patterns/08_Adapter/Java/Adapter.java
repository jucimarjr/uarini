
public class Adapter {

	public static void main(String[] args) {
		Operar soma = new Somar();
		Operar subtracao = new SubtrairAdapter(new Subtrair());
		
		System.out.println("Resultado da soma: " + soma.operar(10, 5));
		System.out.println("Resultado da subtração: " + subtracao.operar(10, 5));
	}
}
