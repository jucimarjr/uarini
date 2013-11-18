public class ProcPing extends Thread {
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
		while (true) {
			synchronized (this) {
				if (mailbox != null) {
					mailbox = null;
					break;
				}
			}
		}
	}
	
	public void run() {
		for (int i = 1; i <= qtdMsg; i++) {
			send(dado);
			if(i!=qtdMsg){
				recv();
			}
		}
		parent.acordar();
	}
}
