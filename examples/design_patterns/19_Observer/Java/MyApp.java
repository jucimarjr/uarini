
public class MyApp {
    public static void main(String[] args) {
 
    	System.out.println("Enter Text >");
    	
        final EventSource eventSource = new EventSource();
 
        final ResponseHandler responseHandler = new ResponseHandler();
 
        eventSource.addObserver(responseHandler);
 
        Thread thread = new Thread(eventSource);
        thread.start();
    }
}