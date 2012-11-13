
public class Computador extends ComponenteComposite{
	
	public double getPrecoCusto() {
		double preco = 0;
		for (Componente c: componentes){
			preco += c.getPrecoCusto();
		}
		return preco;
	}

	public double getPrecoLucroMinimo() {
		double preco = 0;
		for (Componente c: componentes){
			preco += c.getPrecoLucroMinimo();
		}
		return preco;
	}

	public double getPrecoLucroMaximo() {
		double preco = 0;
		for (Componente c: componentes){
			preco += c.getPrecoLucroMaximo();
		}
		return preco;
	}
}
