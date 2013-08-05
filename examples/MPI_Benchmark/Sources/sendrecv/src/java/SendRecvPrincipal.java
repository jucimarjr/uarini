public class SendRecvPrincipal{
	
	public static void main(String[] args) {
		
		int DATA = Integer.parseInt(args[0]);
		int REP = Integer.parseInt(args[1]);		
		int PROC = Integer.parseInt(args[2]);
		
		String localSaida = Salvar.OUT_PATH + "alltoall.txt";
		SendRecv ring = new SendRecv(localSaida, DATA, REP, PROC);
		ring.run();
	}
}