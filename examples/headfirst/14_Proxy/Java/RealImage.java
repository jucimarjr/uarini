//Este fonte foi retirado de: http://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Proxy
//Ultimo acesso em Agosto de 2013

class RealImage implements Image {
 
    private String filename = null;
    
    public RealImage(final String FILENAME) { 
        filename = FILENAME;
        loadImageFromDisk();
    }
 
    private void loadImageFromDisk() {
        System.out.println("Loading   " + filename);
    }
 
    public void displayImage() { 
        System.out.println("Displaying " + filename); 
    } 
}