
public class CommandFactory2 implements CommandFactory{
	
	public BaseCommand create(String nome){
		BaseCommand command = null;
		
		if(nome.equals("CommandEmail")){
			System.out.println("Do Aplicativo 2");
			command = new CommandEmail();
		}
		else if(nome.equals("CommandPDF")){
			System.out.println("Do Aplicativo 2");
			command = new CommandPDF();
		}		
		return command;
	}
}
