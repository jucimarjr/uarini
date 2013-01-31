import java.util.ArrayList;
import java.util.Collection;

public class GerenciadorRemessas {
	Collection<Remessa> remessas = new ArrayList<Remessa>();
	
	public void gerarRemessa(String e){
		Estado estado = EstadoFactory.getEstado(e);
		remessas.add(new Remessa(estado));	
	}
}
