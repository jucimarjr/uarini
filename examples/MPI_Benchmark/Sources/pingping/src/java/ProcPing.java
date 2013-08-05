public class ProcPing extends Thread {
	// public String mailbox = new String();
	public byte[] mailbox;
	private ProcPing peer;
	private PingPing parent;
	private int qtdMsg;
	private byte[] dado;

	public ProcPing(String name, byte[] dado, PingPing parent, int qtdMsg) {
		this.setName(name);
		this.dado = dado;
		this.parent = parent;
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
//					System.out.println("Proc " + this.getName()
//							+ ": mensagem recebida!\n" + "Recebido: \""
//							+ printByteArray(mailbox) + "\"\n");
					mailbox = null;
					break;
				}
			}
//			System.out.println("Passei pelo loop do receive!");
		}
	}

//	private String printByteArray(byte[] bytes){
//		String out = new String();
//		for (int i = 0; i < bytes.length; i++) {
//			out += Byte.toString(bytes[i]);
//		}
//		return out;
//	}
	
	public void run() {
		for (int i = 1; i <= qtdMsg; i++) {
			send(dado);
//			System.out.println("Proc " + this.getName() + ": ping enviado!\n"
//					+ "Esperando ping...\n");
			if(i!=qtdMsg){
				recv();
			}
		}
		parent.acordar();
	}
}
