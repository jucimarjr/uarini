//Este fonte foi retirado de: http://sourcemaking.com/design_patterns/builder/java/2
//Ultimo acesso em Agosto de 2013

public class Pizza {
	  private String dough = "";
	  private String sauce = "";
	  private String topping = "";

	  public void setDough(String dough)     { this.dough = dough; }
	  public void setSauce(String sauce)     { this.sauce = sauce; }
	  public void setTopping(String topping) { this.topping = topping; }

	  public void showIngredients() {
		  System.out.println("Ingredients: " + dough + ", " + sauce + ", " + topping);
	  }
}
