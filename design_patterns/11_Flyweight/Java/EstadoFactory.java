import java.util.HashMap;
import java.util.Map;


public class EstadoFactory {
	private static Map<String, Estado> estados = new HashMap<String, Estado>();
	
	public static Estado getEstado(String nome){
		Estado estado = estados.get(nome);
		if(estados == null){
			estado = new Estado(nome);
			estados.put(nome, estado);
		}
		return estado;
	}
}
