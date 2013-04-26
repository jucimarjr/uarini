public class TestChain {
	public static void main(String[] args) {
		// configure Chain of Responsibility
		Chain c1 = new NegativeProcessor();
		Chain c2 = new ZeroProcessor();
		Chain c3 = new PositiveProcessor();
		c1.setNext(c2);
		c2.setNext(c3);

		// calling chain of responsibility
		c1.process(new Number(99));
		c1.process(new Number(-30));
		c1.process(new Number(0));
		c1.process(new Number(100));
	}
}
