public class ProcPong extends Thread {
	// public String mailbox = new String();
	public boolean espera = false;
	public byte[] mailbox;
	private ProcPing peer;
	private int qtdMsg;
	private byte[] dado;

	public ProcPong(String name, byte[] dado, int qtdMsg) {
		this.setName(name);
		this.dado = dado;
		this.qtdMsg = qtdMsg;
	}

	public void setPeer(ProcPing peer) {
		this.peer = peer;
	}

	private synchronized void send(byte[] msg) {
		peer.mailbox = msg.clone();
	}
	
	private void recv() {
		// verifica mailbox até ter mensagem
		while (true) {
			synchronized (this) {
				if (mailbox != null) {
//					System.out.println("MSG: " + printByteArray(mailbox));
//					mailbox[0] = 1;
					mailbox = null;
					break;
				}
			}
		}
	}
	
	public void run() {
		for (int i = 1; i <= qtdMsg; i++) {
			recv();
//			System.out.println("Proc " + this.getName() + ": ping recebido!\n"
//					+ "Enviando pong...\n");
			send(dado);
		}
	}
	
//	private String printByteArray(byte[] bytes){
//		String out = new String();
//		for (int i = 0; i < bytes.length; i++) {
//			out += Byte.toString(bytes[i]);
//		}
//		return out;
//	}
	
}
