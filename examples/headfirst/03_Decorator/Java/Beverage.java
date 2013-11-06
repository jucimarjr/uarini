//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public abstract class Beverage {
	String description = "Unknown Beverage";
  
	public String getDescription() {
		return description;
	}
 
	public abstract double cost();
}
