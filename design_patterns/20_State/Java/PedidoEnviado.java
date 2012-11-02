
public class PedidoEnviado implements StatusPedido{

	public void alterar(Pedido pedido) {
		System.out.println("Não é possível realizar alterações. O pedido já foi enviado.");
	}

	public void cancelar(Pedido pedido) {
		System.out.println("Pedido enviado cancelado, será realizada a devolução do pedido.");
		pedido.setStatus(new PedidoCancelado());
	}

	public void enviar(Pedido pedido) {
		System.out.println("Pedido já está em processo de envio.");
	}
}
