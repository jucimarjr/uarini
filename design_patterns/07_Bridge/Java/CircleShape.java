class CircleShape extends Shape {
   private double x, y, radius;
   
   public CircleShape(double x, double y, double radius, DrawingAPI drawingAPI) {
      super(drawingAPI);
      this.x = x;  
      this.y = y;  
      this.radius = radius;
   }
 
   public void draw() {
        drawingAPI.drawCircle(x, y, radius);
   }
   
   public void resizeByPercentage(double pct) {
        radius *= pct;
   }
}
