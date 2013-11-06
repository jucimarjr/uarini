//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Interpreter
//Ultimo acesso em Agosto de 2013

public class Variable implements Expression {
	private String name;

	public Variable(String name) {
		this.name = name;
	}

	public int interpret(Map<String, Expression> variables) {
		if (null == variables.get(name))
			return 0;
		return variables.get(name).interpret(variables);
	}
}
