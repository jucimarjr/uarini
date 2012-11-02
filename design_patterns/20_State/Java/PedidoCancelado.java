
public class PedidoCancelado implements StatusPedido{

	public void alterar(Pedido pedido) {
		System.out.println("Não é possível realizar alterações. Este pedido está cancelado.");
	}

	public void cancelar(Pedido pedido) {
		System.out.println("O pedido já foi cancelado.");
	}

	public void enviar(Pedido pedido) {
		System.out.println("Não é possível realizar o envio do pedido. Este pedido está cancelado.");
	}
}
