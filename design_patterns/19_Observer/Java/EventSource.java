import java.util.Observable;          //Observable is here
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
 
public class EventSource extends Observable implements Runnable {
    
	public void run() {
        try {
        	
            final InputStreamReader isr = new InputStreamReader(System.in);
            final BufferedReader br = new BufferedReader(isr);
            
            while (true) {
                String response = br.readLine();
                setChanged();
                notifyObservers(response);
            }
        }
        
        catch (IOException e) {
            e.printStackTrace();
        }
    }
}