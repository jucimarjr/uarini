public class PingPingPrincipal {

	public static void main(String[] args) { 

		int tamMsg = Integer.parseInt(args[0]);
		int qtdMsg = Integer.parseInt(args[1]);

		String localSaida = Salvar.OUT_PATH + "pingping.txt";

		PingPing p = new PingPing(tamMsg, qtdMsg, localSaida);
		p.start();
	}
}
