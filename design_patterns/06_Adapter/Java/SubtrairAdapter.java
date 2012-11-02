
public class SubtrairAdapter implements Operar{
	
	private Subtrair subtrair;
	
	public SubtrairAdapter(Subtrair subtrair) {
		this.subtrair = subtrair;
	}

	public double operar(double A, double B) {
		return subtrair.calcular(A, B);
	}
}
