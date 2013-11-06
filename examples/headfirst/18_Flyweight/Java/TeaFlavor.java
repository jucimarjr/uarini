//Este fonte foi retirado de: http://www.fluffycat.com/Java-Design-Patterns/Flyweight/
//Ultimo acesso em Agosto de 2013

public class TeaFlavor extends TeaOrder {  
    String teaFlavor; 
   
    TeaFlavor(String teaFlavor) {
        this.teaFlavor = teaFlavor;
    }
   
    public String getFlavor() {
        return this.teaFlavor;
    }
   
    public void serveTea(TeaOrderContext teaOrderContext) {
        System.out.println("Serving tea flavor " + 
                             teaFlavor + 
                           " to table number " + 
                             teaOrderContext.getTable());
    }
}
