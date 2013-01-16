public class Main {
	public static void main(String[] args) {
		Conexao con;
		
		Conexao con1 = new DialUp();
		Conexao con2 = new Adsl();
		
		con = con1;
		con.conectar();
		con = con2;
		con.conectar();
	}
}