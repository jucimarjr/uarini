public class Evaluator implements Expression {
	private Expression syntaxTree;
	Stack<Expression> expressionStack = new Stack<Expression>();
	
	public Evaluator(String expression) {
		for (String token : expression.split(" ")) {
			if (token.equals("+")) {
				Expression subExpression = new Plus(expressionStack.pop(),
						expressionStack.pop());
				expressionStack.push(subExpression);
			} else if (token.equals("-")) {
				Expression right = expressionStack.pop();
				Expression left = expressionStack.pop();
				
				Expression subExpression = new Minus(left, right);
				expressionStack.push(subExpression);
			} else
				expressionStack.push(new Variable(token));
		}
		syntaxTree = expressionStack.pop();
	}

	public int interpret(Map<String, Expression> context) {
		return syntaxTree.interpret(context);
	}
}
