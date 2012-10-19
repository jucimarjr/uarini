public class Operar {
	private int a;
	private int b;
	private Operacao op;
	
	public Operar(int A, int B, Operacao Op) {
		this.a = A;
		this.b = B;
		this.op = Op;
	}
	
	public double calcular(){		
		return op.calcular(a, b);
	}	
}
