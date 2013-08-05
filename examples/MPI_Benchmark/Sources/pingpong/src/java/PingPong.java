public class PingPong extends Thread{

	private boolean espera = true;
	
	private long timeSpawn;
	private long timeExec;

	public long timeStart;
	public long timeEnd;

	private int tamDados;
	private int qtdRept;
	private String outLocation;
	
	public PingPong(int tamDados, int qtdMsg, String outLocation) {
		this.tamDados = tamDados;
		this.qtdRept = qtdMsg;
		this.outLocation = outLocation;
	}
	
	public void run() {
		byte[] dado = generateData(tamDados);

		// cria os processos
		
		timeStart = timeMicroSeg();
		ProcPing ping = new ProcPing("1", dado, this, qtdRept);
		ProcPong pong = new ProcPong("2", dado, qtdRept);
		timeEnd = timeMicroSeg();

		// armazena tempo de criação
		timeSpawn = timeEnd - timeStart;
		
		// inicia execução dos processos
		timeStart = timeMicroSeg();
		
		ping.setPeer(pong); // seta o par
		ping.start();
		pong.setPeer(ping); // seta o par
		pong.start();
		
		dormirAteTerminar();

		// após todos terminarem, executa a prox linha
		timeEnd = timeMicroSeg(); // captura tempo atual

		//FINALIZA TESTE
		timeExec = timeEnd - timeStart; // armazena tempo de execução
		
		// escreve a saída
		Salvar.writeResultPeer(outLocation, tamDados, qtdRept, timeExec, timeSpawn);
	}

	private synchronized void dormirAteTerminar() {
		while(espera){
			try {
				wait();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}		
	}

	public synchronized void acordar() {
		espera = false;
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
