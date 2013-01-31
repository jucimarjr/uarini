
public class Status {
	
	public static void main(String[] args) {
		
		Pedido pedido1 = new Pedido();		
		pedido1.setStatus(new PedidoProcessamento());
		
		pedido1.alterar();
		pedido1.cancelar();
		pedido1.enviar();
		
		Pedido pedido2 = new Pedido();
		pedido2.setStatus(new PedidoCancelado());
		
		pedido2.alterar();
		pedido2.cancelar();
		pedido2.enviar();
		
		Pedido pedido3 = new Pedido();
		pedido3.setStatus(new PedidoEnviado());
		
		pedido3.alterar();
		pedido3.cancelar();
		pedido3.enviar();
	}
}
