//Este fonte foi retirado de: http://javapapers.com/design-patterns/chain-of-responsibility-design-pattern/
//Ultimo acesso em Agosto de 2013

public interface Chain {

	public abstract void setNext(Chain nextInChain);

	public abstract void process(Number request);
}
