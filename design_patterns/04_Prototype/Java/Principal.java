
public class Principal {

	public static void main(String[] args) {
		Cliente cliente = new Cliente();
		Documento pdf = cliente.criarDocumento(Cliente.DOCUMENTO_TIPO_DPF);
		Documento ascii = cliente.criarDocumento(Cliente.DOCUMENTO_TIPO_ASCII);	
	}
}
