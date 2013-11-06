//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Proxy
//Ultimo acesso em Agosto de 2013

class ProxyImage implements Image {
 
    private RealImage image = null;
    private String filename = null;
  
    public ProxyImage(final String FILENAME) { 
        filename = FILENAME; 
    }
 
    public void displayImage() {
        if (image == null) {
           image = new RealImage(filename);
        } 
        image.displayImage();
    } 
}