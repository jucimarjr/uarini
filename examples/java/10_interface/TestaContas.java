public class TestaContas {
	public static void main(String[] args) {
		ContaCorrente cc = new ContaCorrente();
		cc.depositar(1200.00);
		cc.sacar(300);
		
		ContaPoupanca cp = new ContaPoupanca();
		cp.depositar(500.50);
		cp.sacar(25);
		
		GeradorExtratos gerador = new GeradorExtratos();
		gerador.geradorConta(cc);
		gerador.geradorConta(cp);
	}
}