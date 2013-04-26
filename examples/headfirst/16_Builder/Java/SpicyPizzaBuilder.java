public class SpicyPizzaBuilder extends PizzaBuilder {
	  public void buildDough()   { 
		  System.out.println("Setting pan baked dough...");
		  pizza.setDough("pan baked"); 
	  }
	  
	  public void buildSauce()   { 
		  System.out.println("Setting hot sauce...");
		  pizza.setSauce("hot"); 
	  }
	  public void buildTopping() { 
		  System.out.println("Setting pepperoni+salami toppings...");
		  pizza.setTopping("pepperoni+salami"); 
	  }
	  
	  public String toString() {
		  String description = new String();
		  description = "Spicy Pizza built!";
		  return (description);
	  }
}
