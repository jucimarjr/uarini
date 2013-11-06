//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Proxy
//Ultimo acesso em Agosto de 2013

class ProxyExample {
 
   public static void main(String[] args) {
        final Image IMAGE1 = new ProxyImage("HiRes_10MB_Photo1");
        final Image IMAGE2 = new ProxyImage("HiRes_10MB_Photo2");     
 
        IMAGE1.displayImage(); // loading necessary
        IMAGE1.displayImage(); // loading unnecessary
        IMAGE2.displayImage(); // loading necessary
        IMAGE2.displayImage(); // loading unnecessary
        IMAGE1.displayImage(); // loading unnecessary
    }
 
}