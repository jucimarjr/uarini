
public class Singleton {
	
	public static void main(String[] args) {
		
		ConfigManager.getInstance().setServerName("Nome do ConfigManager");
		
		String s = ConfigManager.getInstance().getServerName();
		
		ConfigManager configManager = ConfigManager.getInstance();
		String sl = configManager.getServerName();
		
		System.out.println("Nome: " + s);
		System.out.println("Nome: " + sl);
	}
}
