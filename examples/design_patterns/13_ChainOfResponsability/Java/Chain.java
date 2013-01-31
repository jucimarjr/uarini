
public class Chain {
    public static void main(String[] args) {

    	// Build the chain of responsibility
        Logger logger = new StdoutLogger(Logger.DEBUG);
        Logger logger1 = logger.setNext(new EmailLogger(Logger.NOTICE));
        Logger logger2 = logger1.setNext(new StderrLogger(Logger.ERR));
                
        // Handled by StdoutLogger
        logger.message("Entering function y.", Logger.DEBUG);
 
        // Handled by StdoutLogger and EmailLogger
        logger.message("Step1 completed.", Logger.NOTICE);
 
        // Handled by all three loggers
        logger.message("An error has occurred.", Logger.ERR);
    }
}