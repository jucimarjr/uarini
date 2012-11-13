
public class BtnSearch implements Command {

	IMediator med;
	
	public BtnSearch(IMediator m) {
		med = m;
		med.registerSearch(this);
	}
	
	public void setEnabled(boolean b) {
		if(b == true)
			System.out.println("BtnSearch Enabled");
		else
			System.out.println("BtnSearch Disabled");
	}
	
	public void execute() {
		med.search();
	}
}