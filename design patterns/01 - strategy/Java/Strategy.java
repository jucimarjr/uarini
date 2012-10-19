public class Strategy {

	public static void main(String[] args) {
		
		Operacao somar = new Somar();
		Operar soma = new Operar(10, 5, somar);
		double resultadoSoma = soma.calcular();
		
		Operacao subtrair = new Subtrair();
		Operar subtracao = new Operar(10, 5, subtrair);
		double resultadoSubtracao = subtracao.calcular();
		
		System.out.println("Resultado da soma: " + resultadoSoma);
		System.out.println("Resultado da subtracao: " + resultadoSubtracao);	
	}
}
