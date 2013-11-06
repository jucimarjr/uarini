//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/composite/java/2
//Ultimo acesso em Agosto de 2013

public class Row extends Composite{
	
	public Row(int val) {
		super( val ); 
	}   
	
	public void transverse() {
	    System.out.print( "Row" );
	    super.transverse();
	}
}
