abstract class Shape {
	protected DrawingAPI drawingAPI;

	protected Shape(DrawingAPI drawingAPI){
		this.drawingAPI = drawingAPI;
	}

	public abstract void draw();                             
	public abstract void resizeByPercentage(double pct);     
}