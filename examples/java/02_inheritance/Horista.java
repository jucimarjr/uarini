class Horista extends Empregado {  
       public double getSalario() {  
       return -2;  
   }  
   public void calcular() {  
       System.out.println("Salario da superclasse: "+super.getSalario());  
       System.out.println("Salario da subclasse: "+this.getSalario());  
   }  
   public static void main(String[] args) {  
       Horista h = new Horista();  
       h.calcular();  
   }  
}  
