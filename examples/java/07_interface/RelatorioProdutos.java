public class RelatorioProdutos implements IRelatorio {
	public String getSql() {
		return "select * from produtos";
	}
}