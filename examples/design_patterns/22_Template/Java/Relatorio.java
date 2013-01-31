public abstract class Relatorio {
	
	public String montarRelatorio(){		
		return montarCabecalho() + " " + montarCorpo() + " " + montarRodape(); 
	}
	
	public abstract String montarCabecalho();
	public abstract String montarCorpo();
	public abstract String montarRodape();		
}
