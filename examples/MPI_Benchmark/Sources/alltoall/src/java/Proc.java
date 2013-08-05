

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Proc implements Runnable {
	private int nodeId;
	private BlockingQueue<Message> mailbox = new LinkedBlockingQueue<Message>();

	private AllToAll parent;

	private int repetitions;
	private Message msg;
	private Proc[] procList;

	public Proc(int id) {
		this.nodeId = id;}

	public void setRepetitions(int repetitions){
		this.repetitions = repetitions;}

	public void setData(byte[] data){
		this.msg = new Message(nodeId, data);}

	public void setParent(AllToAll parent){
		this.parent = parent;}

	public void setProcList(Proc[] procList){
		this.procList = procList;}

	public int getNodeId(){
		return nodeId;}

	public void send(Message m) {
		mailbox.add(m);}

	public Message recv() throws InterruptedException {
		Message msg = mailbox.take();
		return msg;}

	private void alertEnd(long endTime){
		Object[] tuple = new Object[2];
		tuple[0] = this;
		tuple[1] = endTime;
		Message alert = new Message(nodeId, tuple);
		parent.send(alert);}

	public long getTimeMicro() {
		return System.nanoTime() / 1000;}

	private void scatter(Proc[] procs, Message msg){
		for(Proc p : procs){
			p.send(msg);}}

	private Message[] gather(int n) throws InterruptedException{
		Message[] msgs = new Message[n];
		for(int i=0; i<n; i++){
			msgs[i] = recv();}
		return msgs;}

	public void run() {
		try {
			for(int i=1; i <= repetitions; i++){
				scatter(procList, msg);
				@SuppressWarnings("unused")
				Message[] recvMsgs = gather(procList.length);}
			long endTime = getTimeMicro();
			alertEnd(endTime);} 
		catch (InterruptedException e) {
			System.out.println("Thread " + nodeId + " Interrmopida!");}
	}
}
