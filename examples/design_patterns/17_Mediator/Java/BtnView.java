
public class BtnView implements Command {

	IMediator med;
	
	public BtnView(IMediator m) {
		med = m;
		med.registerView(this);
	}
	
	public void setEnabled(boolean b) {
		if(b == true)
			System.out.println("BtnView Enabled");
		else
			System.out.println("BtnView Disabled");
	}
	
	public void execute() {
		med.view();
	}
}
