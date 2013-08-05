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
		
		nodes[0] = this; // o próprio ring é o primeiro
		
//		nodes[n-1] = new Node(n, nodes[0]);  // último nó
		nodes[n-1] = new Node(nodes[0]);
		executor.execute(nodes[n-1]);
		
		 // são criados n menos um processos, pois o proprio ring é o primeiro!
		// conexao feita de forma inversa para evitar ter que percorrer o vetor duas vezes (criar, conectar)
		for (int i = n-2; i > 0; i--) {
//			nodes[i] = new Node(i + 1, nodes[i+1]); // conecta com o anterior
			nodes[i] = new Node(nodes[i+1]);
			executor.execute(nodes[i]);
		}
		nodes[0].connect(nodes[1]); // conecta o segundo (que foi gerado por último) com o primeiro
	}
	
	// os "this.getAtributo" nesse método foram postos apenas para deixar explícito que é desta classe
	// são atributos da classe pai "Node"
	private void senderNodeMode(){
		try {
			for (int i = 1; i <= NUM_REP; i++) {
//				System.out.println("leva " + i);
//				msg.source = this.getNodeId();
				this.getNextNode().send(msg);

				@SuppressWarnings("unused")
//				Message receivedMsg = recv();
				byte[] receivedMsg = recv();

//				System.out.println("RECEBIDO:   " + receivedMsg.source + "  ->  " + getNodeId());
			}
		} catch (InterruptedException e) {
//			System.out.println("Thread " + this.getNodeId() + " Interrompida!");
			System.out.println("Thread interrompida!!!");
		}
	}
	
	public void run(){
		// variáveis de medição
		long timeStart, timeEnd, timeSpawn, timeExec;

		// geração de dados feita ao criar-se o anel!
//		msg = new Message(0, generateData(TAM_MSG));
		msg = generateData(TAM_MSG);

		timeStart = getTimeMicro();
		spawnAndConnectNodes();
		timeEnd = getTimeMicro();

		timeSpawn = timeEnd - timeStart;

		// Inicia-se o teste!
		// a thread do main se torna a "cabeça" do anel
		timeStart = getTimeMicro();
		senderNodeMode();
		timeEnd = getTimeMicro();

		timeExec = timeEnd - timeStart;
		
		Salvar.writeResultMulti(OUT_LOCATION, TAM_MSG, NUM_PROC, NUM_REP, timeExec, timeSpawn);			
		System.exit(0);
	}
}