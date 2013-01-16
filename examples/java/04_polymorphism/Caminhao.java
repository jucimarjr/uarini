class Caminhao extends Ipva {
	int ano;
	String placa;
	protected static final float vlrBase = (float)1.50;
	private int qtdEixos = 8;
	
	public Caminhao (int ano, String placa) {
		this.ano = ano;
		this.placa = placa;
	}
	
	public float calcularIpva() {
		return (vlrBase * qtdCavalos * qtdEixos);
	}
}