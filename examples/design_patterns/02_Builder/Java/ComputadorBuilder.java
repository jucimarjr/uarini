
public abstract class ComputadorBuilder {
	protected Computador computador;
	
	public abstract void createComputador();
	public abstract void addPlacaMae();
	public abstract void addHardDisk();
	
	public Computador getComputador(){
		return computador;
	}
}
