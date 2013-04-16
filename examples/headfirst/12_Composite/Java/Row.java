public class Row extends Composite{
	
	public Row(int val) {
		super( val ); 
	}   
	
	public void transverse() {
	    System.out.print( "Row" );
	    super.transverse();
	}
}
