public class ExecRelatorio {
	public static void main(String[] args) {
		IRelatorio relatorios;
		
		IRelatorio relatorioCli = new RelatorioClientes();
		IRelatorio relatorioPro = new RelatorioProdutos();
		
		relatorios = relatorioCli;
		System.out.println(relatorios.getSql());
		
		relatorios = relatorioPro;
		System.out.printl(relatorios.getSql());
	}
}