
public class ComputadorIterator {

	private Computador computador;
	private int current;
	private int steps;

	public ComputadorIterator(Computador c){
		this.computador = c;
	}

	public Componente proximoComponente()
	{
		current++;
		steps = 0;
		Componente r = getComponente(computador);
		if (r == null) 
			current--;
		return r;
	}

	public Componente ComponenteAnterior(){
		current--;
		steps = 0;
		Componente r = getComponente(computador);
		if(r == null)
			current++;
		return r;
	}

	private Componente getComponente(ComponenteComposite cl){
		Componente retorno = null;
		for(Componente c: cl.componentes){
			steps++;			
			if(c instanceof ComponenteComposite){
				if(steps == current)
				{
					retorno = c;
					break;
				}
				else {
					Componente cll = getComponente((ComponenteComposite)c);
					if(cll == null && steps == current)
						return null;
					else if(cll  != null && steps == current)
						return cll;
				}
			}
			else  {
				if(steps == current){
					retorno = c;
					break;
				}
			}
		}
		return retorno;
	}
}
