//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/composite/java/2
//Ultimo acesso em Agosto de 2013

public class Primitive implements Component {
	private int value;
	
	public Primitive(int val) {
		value = val; 
	}
	public void transverse(){
		System.out.print( value + "  " ); 
	}
}