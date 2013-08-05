import java.io.*;

public class ArquivoEscrita {
	
	private FileWriter fileStream = null;
	private BufferedWriter out = null;

	public boolean abrir(String local) {
      try{
		fileStream = new FileWriter(local);
        out = new BufferedWriter(fileStream);
        return true;
        
		}catch (Exception e){
			//System.err.println("Error: " + e.getMessage());
			return false;
		}
	}
	
	public void escrever (String conteudo) {
		try {
			out.write(conteudo);
		} catch (Exception e) {
			System.err.println("Error: " + e.getMessage());
		}
	}
	
	public void fechar(){
		try {
			out.close();
	
		} catch (Exception e) {
			System.err.println("Error: " + e.getMessage());
		}
	}
	
}
