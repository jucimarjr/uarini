class LblDisplay {
 
    IMediator med;
 
    LblDisplay(IMediator m) {
        med = m;
        med.registerDisplay(this);        
    }

	public void setText(String string) {
		System.out.println(string);
	}
}