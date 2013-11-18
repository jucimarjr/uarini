public class ProcPong extends Thread {
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
			recv();
			send(dado);
		}
	}
