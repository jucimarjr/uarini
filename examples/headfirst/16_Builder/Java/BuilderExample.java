public class BuilderExample {
	  public static void main(String[] args) {
		    Waiter waiter = new Waiter();
		    PizzaBuilder hawaiianPizzaBuilder = new HawaiianPizzaBuilder();
		    PizzaBuilder spicyPizzaBuilder = new SpicyPizzaBuilder();

		    System.out.println("   Preparing to build Hawaiian Pizza...");
		    waiter.setPizzaBuilder( hawaiianPizzaBuilder );
		    waiter.constructPizza();

		    Pizza pizza1 = waiter.getPizza();
		    System.out.println(hawaiianPizzaBuilder);
		    pizza1.showIngredients();
		    
		    System.out.println("\n   Preparing to build Spicy Pizza...");
		    waiter.setPizzaBuilder(spicyPizzaBuilder);
		    waiter.constructPizza();
		    
		    Pizza pizza2 = waiter.getPizza();
		    System.out.println(spicyPizzaBuilder);
		    pizza2.showIngredients();
	  }
}
