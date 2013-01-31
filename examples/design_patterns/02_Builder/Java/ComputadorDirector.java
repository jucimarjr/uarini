
public class ComputadorDirector {
	
	ComputadorBuilder builder;
	
	public ComputadorDirector(ComputadorBuilder builder){
		this.builder = builder;
	}
	
	public Computador buildComputador(){
		builder.createComputador();
		builder.addPlacaMae();
		builder.addHardDisk();
		return builder.getComputador();
	}
}
