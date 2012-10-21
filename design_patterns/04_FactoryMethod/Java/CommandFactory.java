
public class CommandFactory {
	
	public BaseCommand create(String nome){
		BaseCommand command = null;
		
		if(nome.equals("CommandEmail")){
			command = new CommandEmail();
		}
		else if(nome.equals("CommandPDF")){
			command = new CommandPDF();
		}		
		return command;
	}
}
