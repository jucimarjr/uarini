//Este fonte esta disponivel em: Livro Head First Design Patterns. 
//Autores: Freeman, E., Freeman, E., Sierra, K., and Bates, B. (2004).O'Reilly Media Inc., 01st ed.

public class MuteQuack implements QuackBehaviour{
	public void quack(){
		System.out.println("<< Silence >>");
	}
}
