//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/composite/java/2
//Ultimo acesso em Agosto de 2013

public class Column extends Composite{
	
	public Column(int val) { 
		super( val ); 
	}
	
	public void transverse() {
		System.out.print( "Col" );
	    super.transverse();
	}
}
