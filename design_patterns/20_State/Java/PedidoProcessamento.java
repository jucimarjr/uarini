
public class PedidoProcessamento implements StatusPedido{

	public void alterar(Pedido pedido) {
		System.out.println("O pedido em processamento está sendo alterado.");
	}

	public void cancelar(Pedido pedido) {
		System.out.println("O pedido em processamento foi cancelado.");
		pedido.setStatus(new PedidoCancelado());
	}

	public void enviar(Pedido pedido) {
		System.out.println("O pedido em processamento foi enviado.");
		pedido.setStatus(new PedidoEnviado());
	}
}
