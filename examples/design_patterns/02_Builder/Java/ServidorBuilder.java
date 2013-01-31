
public class ServidorBuilder extends ComputadorBuilder{

	public void createComputador() {
		computador = new Computador();
	}

	public void addPlacaMae() {
		PlacaMae placaMae = new PlacaMae();
		placaMae.add(new Memoria());
		placaMae.add(new CPU());
		computador.add(placaMae);
	}

	public void addHardDisk() {
		computador.add(new HardDisk());
		computador.add(new HardDisk());
		computador.add(new HardDisk());
	}
}
