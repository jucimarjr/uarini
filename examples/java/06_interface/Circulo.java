public class Circulo implements Figura {
	double raio;
	
	public Circulo(double raio) {
		this.raio = raio;
	}
	
	public double calcularArea() {
		double area = 0;
		area = 3.14 * raio * raio;
		return area;
	}
}