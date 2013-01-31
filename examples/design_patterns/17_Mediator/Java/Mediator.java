class Mediator implements IMediator {
 
    BtnView btnView;
    BtnSearch btnSearch;
    BtnBook btnBook;
    LblDisplay show;
 
    public void registerView(BtnView v) {
        btnView = v;
    }
 
    public void registerSearch(BtnSearch s) {
        btnSearch = s;
    }
 
    public void registerBook(BtnBook b) {
        btnBook = b;
    }
 
    public void registerDisplay(LblDisplay d) {
        show = d;
    }
 
    public void book() {
        btnBook.setEnabled(false);
        btnView.setEnabled(true);
        btnSearch.setEnabled(true);
        show.setText("booking...");
    }
 
    public void view() {
        btnView.setEnabled(false);
        btnSearch.setEnabled(true);
        btnBook.setEnabled(true);
        show.setText("viewing...");
    }
 
    public void search() {
        btnSearch.setEnabled(false);
        btnView.setEnabled(true);
        btnBook.setEnabled(true);
        show.setText("searching...");
    }
}