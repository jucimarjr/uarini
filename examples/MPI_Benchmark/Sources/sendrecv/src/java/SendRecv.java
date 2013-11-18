import java.util.concurrent.ExecutorService;

public class SendRecv extends Node{
	private final String OUT_LOCATION;
	private final int NUM_PROC;
	private final int NUM_REP;
	private final int TAM_MSG;
	
	private volatile byte[] msg;
	
	private ExecutorService executor;
	
	public SendRecv(String outLocation, int nos, int threads, int num_rep, int tamMsg, ExecutorService executor) {
		super(null);
		OUT_LOCATION = outLocation;
		NUM_PROC = nos;
		NUM_REP = num_rep;
		TAM_MSG = tamMsg;
		this.executor = executor;
	}

	public long getTimeMicro() {
		return System.nanoTime() / 1000;
	}

	public byte[] generateData(int tamDados) {
		byte[] dado = new byte[tamDados];
		return dado;
	}
	
	public void spawnAndConnectNodes() {
		int n = NUM_PROC;
		Node[] nodes = new Node[n];
		
		nodes[0] = this;
		
		nodes[n-1] = new Node(nodes[0]);
		executor.execute(nodes[n-1]);
		
		for (int i = n-2; i > 0; i--) {
			nodes[i] = new Node(nodes[i+1]);
			executor.execute(nodes[i]);
		}
		nodes[0].connect(nodes[1]);
	}
	
	private void senderNodeMode(){
		try {
			for (int i = 1; i <= NUM_REP; i++) {
				this.getNextNode().send(msg);

				@SuppressWarnings("unused")
				byte[] receivedMsg = recv();
			}
		} catch (InterruptedException e) {
			System.out.println("Thread interrompida!!!");
		}
	}
	
	public void run(){
		long timeStart, timeEnd, timeSpawn, timeExec;

		msg = generateData(TAM_MSG);

		timeStart = getTimeMicro();
		spawnAndConnectNodes();
		timeEnd = getTimeMicro();

		timeSpawn = timeEnd - timeStart;

		timeStart = getTimeMicro();
		senderNodeMode();
		timeEnd = getTimeMicro();

		timeExec = timeEnd - timeStart;
		
		Salvar.writeResultMulti(OUT_LOCATION, TAM_MSG, NUM_PROC, NUM_REP, timeExec, timeSpawn);			
		System.exit(0);
	}
}
