public class PingPing extends Thread{

	private final int QTD_PROC_TOTAL = 2;
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

		timeStart = timeMicroSeg();
		ProcPing p1 = new ProcPing("1", dado, this, qtdRept);
		ProcPing p2 = new ProcPing("2", dado, this, qtdRept);
		timeEnd = timeMicroSeg();

		timeSpawn = timeEnd - timeStart;
		
		timeStart = timeMicroSeg();
		p1.setPeer(p2);
		p1.start();
		p2.setPeer(p1);
		p2.start();
		
		dormirAteTerminar();

		timeEnd = timeMicroSeg();

		timeExec = timeEnd - timeStart;
		
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
	}
}
