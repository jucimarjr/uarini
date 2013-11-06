//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/builder/java/2
//Ultimo acesso em Agosto de 2013

public class HawaiianPizzaBuilder extends PizzaBuilder {
	  public void buildDough()   { 
		  System.out.println("Setting cross dough...");
		  pizza.setDough("cross"); 
	  }
	  public void buildSauce()   { 
		  System.out.println("Setting mild sauce...");
		  pizza.setSauce("mild"); 
	  }
	  public void buildTopping() {
		  System.out.println("Setting ham+pineapple toppings...");
		  pizza.setTopping("ham+pineapple");
	  }
	  
	  public String toString() {
		  String description = new String();
		  description = "Hawaiian Pizza built!";
		  return (description);
	  }
}