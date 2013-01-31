
public class Command {
	
	public static void main(String[] args) {
		
		Operar soma = new Somar();
		Operar subtair = new Subtrair();
		
		System.out.println(soma.operar());
		System.out.println(subtair.operar());		
	}
}
