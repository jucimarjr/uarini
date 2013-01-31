class MediatorDemo{
 

    public static void main(String[] args) {
        IMediator med = new Mediator();
        
        BtnBook btnBook = new BtnBook(med);
    	BtnSearch btnSearch = new BtnSearch(med);
    	BtnView btnView = new BtnView(med);
    	LblDisplay label = new LblDisplay(med);
    	
    	med.book();
    	med.search();
    	med.view();
    	label.setText("Hello Word");
    }
}
