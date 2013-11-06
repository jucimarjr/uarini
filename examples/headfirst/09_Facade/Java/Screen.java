//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class Screen {
	String description;
	
	public Screen(String description) {
		this.description = description;
	}
 
	public void up() {
		System.out.println(description + " going up");
	}
 
	public void down() {
		System.out.println(description + " going down");
	}
 
  
        public String toString() {
                return description;
        }
}
