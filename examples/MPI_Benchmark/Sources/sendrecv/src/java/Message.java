public class Message {	
	Node Proc;
	byte[] message;
	
	public Message(Node Proc, int DataSize) {
		this.Proc = Proc;
		this.message = new byte[DataSize];
	}
}
