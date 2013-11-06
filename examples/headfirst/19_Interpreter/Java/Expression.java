//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Interpreter
//Ultimo acesso em Agosto de 2013

public interface Expression {
	public int interpret(Map<String, Expression> variables);
}
