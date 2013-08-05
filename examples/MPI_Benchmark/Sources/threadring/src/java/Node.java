import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Node implements Runnable {
//	private int nodeId;
	private Node nextNode;
//	private BlockingQueue<Message> mailbox = new LinkedBlockingQueue<Message>();
	private BlockingQueue<byte[]> mailbox = new LinkedBlockingQueue<byte[]>();
	
//	public Node(int id, Node nextNode) {
//		this.nodeId = id;
	public Node(Node nextNode) {
		this.nextNode = nextNode;
	}

	public Node getNextNode(){
		return this.nextNode;
	}
	
/*	
    public int getNodeId(){
		return nodeId;
	}
*/
	public void connect(Node node) {
		this.nextNode = node;
	}

//	public void send(Message m) {
	public void send(byte[] m) {
		mailbox.add(m);
		// executor.execute(this);
	}

//	public Message recv() throws InterruptedException {
//	Message msg = mailbox.take();
	public byte[] recv() throws InterruptedException {
		byte[] msg = mailbox.take();
		return msg;
	}

	public void run() {
		try {
			while (true) {
//				Message msg = recv();
				byte[] msg = recv();
//				System.out.println("RECEBIDO:   " + msg.source + "  ->  " + nodeId);
//				msg.source = nodeId;
				nextNode.send(msg);
			}
		} catch (InterruptedException e) {
//			System.out.println("Thread " + nodeId + " Interrmopida!");
			System.out.println("Thread Interrompida!!");
		}
	}
}
