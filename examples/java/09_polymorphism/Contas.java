public class Contas {
	public static void mostrarCalculo (OperacaoMatematica operacao, double x, double y) {
		System.out.println("O resultado eh: " + operacao.calcular(x,y));
	}
	
	public static void main(String[] args) {
		Contas.mostrarCalculo(new Soma(), 5, 2);
		Contas.mostrarCalculo(new Subtracao(), 5, 2);
	}
}