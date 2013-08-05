public class PingPongPrincipal {

	public static void main(String[] args) { 
		int tamMsg = Integer.parseInt(args[0]);
		int qtdMsg = Integer.parseInt(args[1]);
		
		//String localSaida = "/home/pesquisador/temp/out_java_pingpong.txt";
		String localSaida = Salvar.OUT_PATH + "pingpong.txt";
		
		PingPong p = new PingPong(tamMsg, qtdMsg, localSaida);
		p.start();
	}
	
}
