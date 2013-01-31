import java.util.Map;

class Variable implements Expression {

	private String name;

	public Variable(String name){ 
		this.name = name;
	}

	public int interpret(Map<String,Expression> variables)  { 
		if(null==variables.get(name)) 
			return 0;
		return variables.get(name).interpret(variables); 
	}
}