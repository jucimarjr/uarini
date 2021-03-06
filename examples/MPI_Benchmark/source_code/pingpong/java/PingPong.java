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

		timeStart = timeMicroSeg();
		ProcPing ping = new ProcPing("1", dado, this, qtdRept);
		ProcPong pong = new ProcPong("2", dado, qtdRept);
		timeEnd = timeMicroSeg();

		timeSpawn = timeEnd - timeStart;
		
		timeStart = timeMicroSeg();
		
		ping.setPeer(pong);
		ping.start();
		pong.setPeer(ping);
		pong.start();
		
		dormirAteTerminar();
		timeEnd = timeMicroSeg();

		timeExec = timeEnd - timeStart;
		
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
	}
}
