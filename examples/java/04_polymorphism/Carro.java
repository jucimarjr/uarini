class Carro extends Ipva {
	int ano;
	String placa;
	protected static final float vlrBase = (float)1.20;

	public Carro (int ano, String placa) {
		this.ano = ano;
		this.placa = placa;
	}
	
	public float calcularIpva() {
		return (vlrBase * qtdCavalos);
	}
}