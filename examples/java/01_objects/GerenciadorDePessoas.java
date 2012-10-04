public class GerenciadorDePessoas {  
  public static void main(String args[]) {  
    
	Pessoa pVitor;  
    pVitor = new Pessoa("Vitor Fernando Pamplona","07/11/1983");   
      
    pVitor.receber(1000.00);   
      
    Pessoa pJoão = new Pessoa("João da Silva", "18/02/1970");   
      
    pJoao.receber(500.00);  
    pJoao.gastar(100.00);  
  }  
}
