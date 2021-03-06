//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Interpreter
//Ultimo acesso em Agosto de 2013

public class Plus implements Expression {
    Expression leftOperand;
    Expression rightOperand;
    public Plus(Expression left, Expression right) { 
        leftOperand = left; 
        rightOperand = right;
    }
 
    public int interpret(Map<String,Expression> variables)  { 
        return leftOperand.interpret(variables) + rightOperand.interpret(variables);
    }
}
