public class PingPingPrincipal {

	public static void main(String[] args) { 

		// arg[0] -> tam bytes!
		// arg[1] -> quantidade de mensagens!
		
		int tamMsg = Integer.parseInt(args[0]);
		int qtdMsg = Integer.parseInt(args[1]);
		
		//String localSaida = "/home/pesquisador/temp/out_java_pingping.txt";
		String localSaida = Salvar.OUT_PATH + "pingping.txt";
		
		PingPing p = new PingPing(tamMsg, qtdMsg, localSaida);
		p.start();
	}
	
}