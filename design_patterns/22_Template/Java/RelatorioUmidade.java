
public class RelatorioUmidade extends Relatorio{
	
	public String montarCabecalho() {
		return "Relatório de Umidade";
	}

	public String montarCorpo() {
		return "Apresenta a escala de umidade";
	}

	public String montarRodape() {
		return "Data do acontecimento da umidade";
	}
}
