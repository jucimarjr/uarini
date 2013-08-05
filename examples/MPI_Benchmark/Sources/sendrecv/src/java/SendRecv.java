import java.util.Arrays;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;


public class SendRecv implements Runnable{

	String OUT_LOCATION;
	private int DataSize;
	private int Proc;
	private int Rep;
	private int Count;

	private BlockingQueue<Integer> mailbox = new LinkedBlockingQueue<Integer>();

	public SendRecv(String outLocayion, int DataSize, int Proc, int Rep){
		this.OUT_LOCATION = outLocayion;
		this.DataSize = DataSize;
		this.Proc = Proc;
		this.Rep = Rep;
		this.Count = 0;
	}
	
	public long getTimeMicro() {
		return System.nanoTime() / 1000;
	}
	
	private long sum(long[] list){
		long result = 0;
		for(long elem : list){
			result += elem;
		}
		return result;
	}
	
	public void sendMessage(Integer j){
		mailbox.add(j);
	}
	
	public Integer receiveMessage() throws InterruptedException{
		return mailbox.take();
	}

	public void run() {		
		long spawnStart, spawnEnd, timeSpawn, execStart, timeMin, timeMax, timeAvg;
		long[] endTimeList, timeList;
		
		Node[] nodes = new Node[Proc];		
		
		spawnStart = getTimeMicro();		
		for(int i = 0; i< Proc; i++){
			nodes[i] = new Node(Rep, DataSize, this);
		}

		for(int i = 0; i < Proc-1; i++){
			nodes[i].setRightPeer(nodes[i+1]);
		}
		nodes[Proc-1].setRightPeer(nodes[0]);
		spawnEnd = getTimeMicro();
		timeSpawn = spawnEnd - spawnStart;;
		
		execStart = getTimeMicro();
		for(int i = 0; i< Proc; i++){
			nodes[i].start();
		}
		endTimeList = new long[Proc];
		try{
			while(Count != Proc ){
				receiveMessage();
				endTimeList[Count] = getTimeMicro(); 
				Count++;			
			}	
		}
		catch(InterruptedException e){
			System.out.println("Thread Interrompida!!");
		}
		
		timeList = new long[Proc];
		for (int i=0; i < Proc; i++){
			timeList[i] = endTimeList[i] - execStart;
		}
		Arrays.sort(timeList);
		timeMin = timeList[0];
		timeMax = timeList[Proc-1];
		timeAvg = sum(timeList)/timeList.length;
		System.out.println("passou");
		Salvar.writeResultAlltoall(OUT_LOCATION,DataSize, Rep, Proc,timeMin,timeMax,timeAvg, timeSpawn);		
		System.exit(0);
	}
}