
public class AbstractFactory {

	public static void main(String[] args) {
		
		Aplicativo aplicativo1 = new Aplicativo(new CommandFactory1());
		
		Aplicativo aplicativo2 = new Aplicativo(new CommandFactory2());
		
		aplicativo1.buttonEmail_clicked();
		aplicativo1.buttonPDF_clicked();
		
		aplicativo2.buttonEmail_clicked();
		aplicativo2.buttonPDF_clicked();
	}
}
