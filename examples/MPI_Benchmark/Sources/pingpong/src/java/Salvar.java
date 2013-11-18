public class Salvar {
	public static String OUT_PATH = "../../docs/java/out_java_";
	
	public static void writeResultAlltoall(String outLocation, int tamDados, int qtdRept, int qtdProcs, long timeMin, long timeMax, long timeAvg, long timeSpawn){
		String header = String.format("%-10s\t %-15s\t %-15s\t %-17s\t %-17s\t %-17s\t %-17s\t%n",
									"#bytes", "#repetitions", "#processes", "t_min[usec]", "t_max[usec]", "t_avg[usec]", "spawn_time[usec]");
		String result = String.format("%-9d\t %-14d\t %-14d\t %-16d\t %-16d\t %-16d\t %-16d\t%n",
									tamDados, qtdRept, qtdProcs, timeMin, timeMax, timeAvg,timeSpawn);
		
		writeResult(outLocation, header, result);		
	}
	
	public static void writeResultMulti(String outLocation, int tamDados, int qtdProcs, int qtdRept, long timeExec, long timeSpawn) {
		String header = String.format("%-13s %-13s %-12s %-16s %-12s %n", "bytes", "processos", "repeticoes", "tempo exec[usec]", "tempo de spawn[usec]");
		String result = String.format("%-13d\t %-12d\t %-12d\t %-15d\t %-20d\t %n", tamDados, qtdProcs, qtdRept, timeExec, timeSpawn); 
		
		writeResult(outLocation, header, result);
	}
	
	public static void writeResultPeer(String outLocation, int tamDados, int qtdRept, long timeExec, long timeSpawn){
		String header = String.format("%-13s\t %-12s\t %-15s\t %-12s\t %n", "bytes", "repeticoes", "t[usec]", "tempo de spawn[usec]");
		String result = String.format("%-13d\t %-12d\t %-14d\t %-20d\t %n",tamDados, qtdRept,timeExec,timeSpawn);
		
		writeResult(outLocation, header, result);
	}
	
	private static void writeResult(String outLocation, String header, String result){
		ArquivoLeitura arq = new ArquivoLeitura();
		if (!arq.abrir(outLocation)) {
			ArquivoEscrita arqOut = new ArquivoEscrita();
			arqOut.abrir(outLocation);
			arqOut.escrever(header);
			arqOut.fechar();
		}

		concatenar(outLocation, result);		
	}
	
	private static boolean concatenar(String local, String conteudo){
		ArquivoLeitura arqIn = new ArquivoLeitura();
		if(arqIn.abrir(local)){
			String strIn = arqIn.lerTudo();
			arqIn.fechar();
			
			String strOut = strIn + conteudo;

			ArquivoEscrita arqOut = new ArquivoEscrita();
			arqOut.abrir(local);
			arqOut.escrever(strOut);
			arqOut.fechar();
			return true;
		}else{
			return false;
		}
	}
}
