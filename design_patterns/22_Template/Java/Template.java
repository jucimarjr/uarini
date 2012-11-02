
public class Template {
	
	public static void main(String[] args) {
	
		Relatorio terremoto = new RelatorioTerremoto();
		Relatorio umidade = new RelatorioUmidade();
		
		System.out.println(terremoto.montarRelatorio());
		System.out.println(umidade.montarRelatorio());
	}
}
