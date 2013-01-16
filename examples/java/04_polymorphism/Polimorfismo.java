public class Polimorfismo {
	public static void main(String[] args) {
		Ipva ipva;
		
		Caminhao caminhao = new Caminhao(1995, "JXT - 1234");
		Carro carro = new Carro(2008, "MNO - 4321");
		
		ipva = caminhao;
		System.out.println("Ipva do caminhao: " + ipva.calcularIpva());
		ipva = carro;
		System.out.println("Ipva do carro: " + ipva.calcularIpva());
	}
}