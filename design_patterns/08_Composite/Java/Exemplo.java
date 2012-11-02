public class Exemplo {

	public static void main(String[] args) {
		
		Computador c1 = new Computador();
		
		PlacaMae placaMae = new PlacaMae();
		
		Memoria memoria1 = new Memoria();
		Memoria memoria2 = new Memoria();
		CPU cpu = new CPU();
				
		placaMae.add(memoria1);
		placaMae.add(memoria2);
		placaMae.add(cpu);
		c1.add(placaMae);
		
		HardDisk hd1 = new HardDisk();
		HardDisk hd2 = new HardDisk();
		
		c1.add(hd1);
		c1.add(hd2);
		
		System.out.println("Calculando pre�o de custo da composi��o:" );
		System.out.println("Valor: " + c1.getPrecoCusto());
		
		System.out.println("Calculando pre�o com lucro m�nimo:" );
		System.out.println("Valor: " + c1.getPrecoLucroMinimo());
		
		System.out.println("Calculando pre�o com lucro m�ximo:" );
		System.out.println("Valor: " + c1.getPrecoLucroMaximo()); 
	}
}