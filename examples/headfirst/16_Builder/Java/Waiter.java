//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/builder/java/2
//Ultimo acesso em Agosto de 2013

public class Waiter {
	  private PizzaBuilder pizzaBuilder;

	  public void setPizzaBuilder(PizzaBuilder pb) { pizzaBuilder = pb; }
	  public Pizza getPizza() { return pizzaBuilder.getPizza(); }

	  public void constructPizza() {
	    pizzaBuilder.createNewPizzaProduct();
	    pizzaBuilder.buildDough();
	    pizzaBuilder.buildSauce();
	    pizzaBuilder.buildTopping();
	  }
}
