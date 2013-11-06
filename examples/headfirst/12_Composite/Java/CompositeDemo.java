//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/composite/java/2
//Ultimo acesso em Agosto de 2013

public class CompositeDemo {

	public static void main(String[] args) {
		Composite first  = new Row( 1 );          // Row1
		Composite second = new Column( 2 );       //   |
	    Composite third  = new Column( 3 );       //   +-- Col2
	    Composite fourth = new Row( 4 );          //   |     |
	    Composite fifth  = new Row( 5 );          //   |     +-- 7
	    first.add( second );                      //   +-- Col3
	    first.add( third  );                      //   |     |
	    third.add( fourth );                      //   |     +-- Row4
	    third.add( fifth  );                      //   |     |     |
	    first.add(  new Primitive( 6 ) );         //   |     |     +-- 9
	    second.add( new Primitive( 7 ) );         //   |     +-- Row5
	    third.add(  new Primitive( 8 ) );         //   |     |     |
	    fourth.add( new Primitive( 9 ) );         //   |     |     +-- 10
	    fifth.add(  new Primitive(10 ) );         //   |     +-- 8
	    first.transverse();                       //   +-- 6
	}
}
