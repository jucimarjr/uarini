import java.util.ArrayList;
import java.util.Collection;


public abstract class ComponenteComposite extends Componente{
	
	protected Collection<Componente> componentes = new ArrayList<Componente>();
	
	public ComponenteComposite(){
		
	}
	
	public void add (Componente c){
		componentes.add(c);
	}
}