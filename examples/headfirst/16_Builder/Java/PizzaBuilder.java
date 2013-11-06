//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/builder/java/2
//Ultimo acesso em Agosto de 2013

public abstract class PizzaBuilder {
	  public Pizza pizza;

	  public Pizza getPizza() { return pizza; }
	  public void createNewPizzaProduct() { pizza = new Pizza(); }

	  public abstract void buildDough();
	  public abstract void buildSauce();
	  public abstract void buildTopping();
}
