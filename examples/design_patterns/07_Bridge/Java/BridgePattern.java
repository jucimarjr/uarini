class BridgePattern {
	public static void main(String[] args) {
		CircleShape Shape1 = new CircleShape(1, 2, 3, new DrawingAPI1());
		CircleShape Shape2 = new CircleShape(1, 4, 5, new DrawingAPI1());

		Shape1.resizeByPercentage(2.5);
		Shape1.draw();

		Shape2.resizeByPercentage(2.5);
		Shape2.draw();
	}
}
