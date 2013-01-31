class BtnBook implements Command {

	IMediator med;

	BtnBook(IMediator m) {
		med = m;
		med.registerBook(this);
	}

	public void execute() {
		med.book();
	}

	public void setEnabled(boolean b) {
		if(b == true)
			System.out.println("BtnBook Enabled");
		else
			System.out.println("BtnBook Disabled");
	}
}