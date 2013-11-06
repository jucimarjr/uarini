//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Interpreter
//Ultimo acesso em Agosto de 2013

public class InterpreterExample {
	public static void main(String[] args) {
		String expression = "w x z - +";
		Evaluator sentence = new Evaluator(expression);
		Map<String, Expression> variables = new HashMap<String, Expression>();
		variables.put("w", new Number(5));
		variables.put("x", new Number(10));
		variables.put("z", new Number(42));
		int result = sentence.interpret(variables);
		System.out.println("Result of sentence is: " + result);
	}
}
