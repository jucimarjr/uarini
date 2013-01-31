public class PlacaMae extends ComponenteComposite{
	
	public double getPrecoCusto() {
		System.out.println("Calculando Preco de custo da composição:");
		double preco = 100;
		for(Componente c: componentes) {
			preco += c.getPrecoCusto();
		}
		return preco;
	}

	public double getPrecoLucroMinimo() {
		System.out.println("Calculando Preco com lucro mínimo da composição:");
		double preco = 150;
		for(Componente c: componentes) {
			preco += c.getPrecoLucroMinimo();
		}
		return preco;
	}

	public double getPrecoLucroMaximo() {
		System.out.println("Calculando Preco com lucro maximo da composição:");
		double preco = 200;
		for(Componente c: componentes) {
			preco += c.getPrecoLucroMaximo();
		}
		return preco;
	}
}