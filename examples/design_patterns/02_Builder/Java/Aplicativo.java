
public class Aplicativo {

	public static void main(String[] args) {
		ServidorBuilder builder =  new ServidorBuilder();
		ComputadorDirector director = new ComputadorDirector(builder);
		Computador computador = director.buildComputador();
		
		computador.getPrecoLucroMaximo();		
	}
}
