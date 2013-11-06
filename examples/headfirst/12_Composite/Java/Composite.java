//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/composite/java/2
//Ultimo acesso em Agosto de 2013

public class Composite implements Component{
	   private Component[] children = new Component[9];  
	   private int total;
	   private int value;
	   
	   public Composite(int val) { 
		   value = val; 
		   total = 0;
	   }
	   
	   public void add(Component c) { 
		   children[total++] = c; 
	   } 
	   
	   public void transverse() {                                 
	      System.out.print( value + "  " );
	      for (int i=0; i < total; i++)
	          children[i].transverse();   
	}
}
