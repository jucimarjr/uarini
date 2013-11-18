import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Node implements Runnable {
	private Node nextNode;
	private BlockingQueue<byte[]> mailbox = new LinkedBlockingQueue<byte[]>();
	
	public Node(Node nextNode) {
		this.nextNode = nextNode;
	}

	public Node getNextNode(){
		return this.nextNode;
	}
	
	public void connect(Node node) {
		this.nextNode = node;
	}

	public void send(byte[] m) {
		mailbox.add(m);
	}

	public byte[] recv() throws InterruptedException {
		byte[] msg = mailbox.take();
		return msg;
	}

	public void run() {
		try {
			while (true) {
				byte[] msg = recv();
				nextNode.send(msg);
			}
		} catch (InterruptedException e) {
			System.out.println("Thread Interrompida!!");
		}
	}
}
