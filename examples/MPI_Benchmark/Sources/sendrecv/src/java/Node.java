import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Node extends Thread{

	int REP;
	int DATA;
	Node RightPeer;	
	SendRecv finalize;
	
	private BlockingQueue<Message> mailbox = new LinkedBlockingQueue<Message>();

	public Node(int REP, int DATA, SendRecv finalize) {
		this.REP = REP;
		this.DATA = DATA;
		this.finalize = finalize;
	}

	public void setRightPeer(Node rightPeer) {
		RightPeer = rightPeer;
	}

	public Node getRightPeer() {
		return RightPeer;
	}

	public void sendMessage(Message m){
		RightPeer.mailbox.add(m);
	}

	public Message receiveMessage() throws InterruptedException{
		return mailbox.take();
	}

	public void run() {
		Message m = new Message(this, DATA);		
		sendMessage(m);
		try{
			while(true){
				Message msg = receiveMessage();
				if(msg.Proc == this){
					REP = REP -1;
				}
				if(REP == 0){
					finalize.sendMessage(new Integer(1));
				}
				else {
					sendMessage(msg);
				}
			}
		}
		catch(InterruptedException e){
			System.out.println("Thread Interrompida!!");
		}
	}
}
