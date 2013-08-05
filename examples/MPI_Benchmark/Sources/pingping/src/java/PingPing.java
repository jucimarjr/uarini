public class PingPing extends Thread{

	private final int QTD_PROC_TOTAL = 2;
	// a medida que os processos finalizarem
	// qtdTerminados é incrementados
	private int qtdProcTerminados = 0;
	
	private long timeSpawn;
	private long timeExec;

	public long timeStart;
	public long timeEnd;

	private int tamDados;
	private int qtdRept;
	private String outLocation;
	
	public PingPing(int tamDados, int qtdMsg, String outLocation) {
		this.tamDados = tamDados;
		this.qtdRept = qtdMsg;
		this.outLocation = outLocation;
	}

	public void run() {
		byte[] dado = generateData(tamDados);

		// cria os processos
		
		timeStart = timeMicroSeg();
		ProcPing p1 = new ProcPing("1", dado, this, qtdRept);
		ProcPing p2 = new ProcPing("2", dado, this, qtdRept);
		timeEnd = timeMicroSeg();

		// armazena tempo de criação
		timeSpawn = timeEnd - timeStart;
		
		// inicia execução dos processos
		timeStart = timeMicroSeg();
		p1.setPeer(p2); // seta o par
		p1.start();
		p2.setPeer(p1); // seta o par
		p2.start();
		
		dormirAteTerminar();

		// após todos terminarem, executa a prox linha
		timeEnd = timeMicroSeg(); // captura tempo atual

		//FINALIZA TESTE
		timeExec = timeEnd - timeStart; // armazena tempo de execução
		
		// escreve a saída
		Salvar.writeResultPeer(outLocation, tamDados, qtdRept, timeExec, timeSpawn);
	}

	private synchronized void dormirAteTerminar() {
		while(qtdProcTerminados!= QTD_PROC_TOTAL){
			try {
				wait();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}		
	}

	public synchronized void acordar() {
		qtdProcTerminados++;
		notifyAll();
	}

	private byte[] generateData(int tamDados) {
		byte[] dado = new byte[tamDados];
		return dado;
	}

	private long timeMicroSeg() {
		return System.nanoTime()/1000;
		//return (Calendar.getInstance().getTimeInMillis() * 1000);

	}

}
