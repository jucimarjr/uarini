import java.util.Arrays;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService; 
import java.util.concurrent.LinkedBlockingQueue;

public class AllToAll{
	String OUT_LOCATION;
	private final int NUM_PROC,NUM_REP,TAM_MSG;
	private BlockingQueue<Message> mailbox = new LinkedBlockingQueue<Message>();
	Message msg;
	ExecutorService executor;

	public AllToAll(String outLocation,int num_proc,int threads,int num_rep,int tamMsg,
			ExecutorService executor){
		OUT_LOCATION = outLocation;
		NUM_PROC = num_proc;
		NUM_REP = num_rep;
		TAM_MSG = tamMsg;
		this.executor = executor;
	}

	public void send(Message m) {
		mailbox.add(m);
	}
	public long getTimeMicro() {
		return System.nanoTime() / 1000;
	}
	public byte[] generateData(int tamDados) {
		byte[] dado = new byte[tamDados];
		return dado;
	}
	public Proc[] spawnProcs(int n) {
		Proc[] procList = new Proc[n];
		for (int i = 0; i < n; i++) {
			procList[i] = new Proc(i+1);
		}
		return procList;
	}

	private long[] finalize(int n){
		try {   long[] endTimeList = new long[n];
		for(int i=0; i < n; i++){
			Message recvMsg = mailbox.take();
			Object[] tuple = (Object[]) recvMsg.value;
			long endTime = (Long) tuple[1];
			endTimeList[i] = endTime;
		}
		return endTimeList;			
		} 
		catch (InterruptedException e) {
			System.out.println("Thread AllToAll Interrmopida!");
			return null;
		}
	}

	private long sum(long[] list){
		long result = 0;
		for(long elem : list){
			result += elem;
		}
		return result;
	}

	public void run(){
		long spawnStart, spawnEnd, timeSpawn, execStart, timeMin, timeMax, timeAvg;
		long[] endTimeList, timeList;

		msg = new Message(0, generateData(TAM_MSG));
		spawnStart = getTimeMicro();
		Proc[] procList = spawnProcs(NUM_PROC);
		spawnEnd = getTimeMicro();
		timeSpawn = spawnEnd - spawnStart;

		execStart = getTimeMicro();
		for (Proc proc : procList) {
			proc.setRepetitions(NUM_REP);
			proc.setData(((byte[]) msg.value).clone());
			proc.setParent(this);
			proc.setProcList(procList.clone());
			executor.execute(proc);
		}
		endTimeList = finalize(NUM_PROC);
		timeList = new long[NUM_PROC];
		for (int i=0; i < NUM_PROC; i++){
			timeList[i] = endTimeList[i] - execStart;
		}
		Arrays.sort(timeList);
		timeMin = timeList[0];
		timeMax = timeList[NUM_PROC-1];
		timeAvg = sum(timeList)/timeList.length;
		Salvar.writeResultAlltoall(OUT_LOCATION,TAM_MSG,NUM_REP,NUM_PROC,timeMin,timeMax,timeAvg, timeSpawn);
		System.exit(0);
	}
}
