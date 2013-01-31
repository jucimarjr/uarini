
public class PlacaMae extends ComponenteComposite{

	public double getPrecoCusto() {
		double preco = 100;
		for (Componente c: componentes){
			preco += c.getPrecoCusto();
		}
		return preco;
	}

	public double getPrecoLucroMinimo() {
		double preco = 150;
		for (Componente c: componentes){
			preco += c.getPrecoLucroMinimo();
		}
		return preco;
	}

	public double getPrecoLucroMaximo() {
		double preco = 200;
		for (Componente c: componentes){
			preco += c.getPrecoLucroMaximo();
		}
		return preco;
	}
}