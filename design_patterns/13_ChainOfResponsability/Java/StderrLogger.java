
class StderrLogger extends Logger {

	public StderrLogger(int mask) {
		this.mask = mask;
	}

	protected void writeMessage(String msg) {
		System.err.println("Sending to stderr: " + msg);
	}
}
