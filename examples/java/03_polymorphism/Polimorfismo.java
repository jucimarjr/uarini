public class Polimorfismo {  
   public static void main(String[] args) {  
      Empregado e;  
      Horista h = new Horista();  
      Mensalista m = new Mensalista();  
      h.setSalario( 2240.00 );  
      m.setSalario( 2240.00 );  
      e = h;  
      System.out.println( "Salario em horas: "+e.getSalario() );  
      e = m;  
      System.out.println( "Salario em horas: "+e.getSalario() );  
   }  
}
