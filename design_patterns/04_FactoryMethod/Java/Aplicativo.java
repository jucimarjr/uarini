
public class Aplicativo {
	
	private CommandFactory commandFactory;
	
	public Aplicativo()	{
		commandFactory = new CommandFactory();		
	}
	
	public void buttonEmail_clicked(){
		BaseCommand comandoEnviar = commandFactory.create("CommandEmail");
		comandoEnviar.executar();
	}

	public void buttonPDF_clicked(){
		BaseCommand comandoEnviar = commandFactory.create("CommandPDF");
		comandoEnviar.executar();
	}
}
