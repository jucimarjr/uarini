public class RelatorioClientes implements IRelatorio {
	public String getSql() {
		return "select * from clientes";
	}
}