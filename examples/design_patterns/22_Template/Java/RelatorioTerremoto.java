
public class RelatorioTerremoto extends Relatorio{

	public String montarCabecalho() {
		return "Relatório de Terremoto";
	}

	public String montarCorpo() {
		return "Apresenta a escala de terretomo";
	}

	public String montarRodape() {
		return "Data do acontecimento";
	}
}
