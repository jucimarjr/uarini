public class Primitive implements Component {
	private int value;
	
	public Primitive(int val) {
		value = val; 
	}
	public void transverse(){
		System.out.print( value + "  " ); 
	}
}