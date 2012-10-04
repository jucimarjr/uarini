public class Pessoa {  
  String nome;  
  String nascimento;  
  double dinheiroNaCarteira;  
    
  public Pessoa(String nome, String nasc) {  
    this.nome = nome;  
    this.nascimento = nasc;  
  }  
    
  public void gastar(double valor) {  
    dinheiroNaCarteira -= valor;  
  }  
    
  public void receber(double valor) {  
    dinheiroNaCarteira += valor;  
  }  
}
