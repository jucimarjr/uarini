

public class Message {

	public volatile Object value;
	public int source;
	
	public Message(int source, Object value) {
		this.source = source;
		this.value = value;
	}
}
