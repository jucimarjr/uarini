import actors.Actor
import actors.Actor._
import java.io._
import scala.io.Source

object sendrecv {
 
	def main(args: Array[String]) {

		var DataSize : Int = Integer.parseInt(args(0))
		var Rep : Int = Integer.parseInt(args(1))
		var NProc : Int = Integer.parseInt(args(2))
		var FileName = args(3)	
		var Data = new Array[Byte](DataSize)
	
		var i = 0;
		while(i < DataSize)
		{
			Data(i) = 1;
			i = i +1;
		}
	
		var Finalize = new Finalize(DataSize, Rep, NProc, FileName)
	
		var ProcInicial = new ProcInitial(Data, Rep, Finalize)	
		ProcInicial.start()

		var ArrayProc = new Array[Proc](NProc -1)
		ArrayProc(NProc -2) = new Proc(Data, Rep, ProcInicial, Finalize)
		ArrayProc(NProc -2).start()
	
		i = NProc - 3
		while(i > -1)
		{
			ArrayProc(i) = new Proc(Data, Rep, ArrayProc(i + 1), Finalize)
			ArrayProc(i).start()
			i = i - 1;
		}
		Finalize.start
		ProcInicial ! ArrayProc(0) 
	}
}

class Finalize(var DataSize: Int, var Rep: Int, var NProc: Int, var FileName: String) extends Actor {
	def act(){		
		var start = System.nanoTime()
		var N : Int = NProc
		var finish: Long = 0
		var sub: Long = 0
		loop{
			react{
				case 1 =>{
					N = N - 1
					finish = System.nanoTime
					sub = sub + finish - start
					if(N == 0)
					{
						sub = sub/NProc
						try {
							var source = scala.io.Source.fromFile("../../docs/scala/" + FileName)
							var lines = source .mkString
							lines = lines + DataSize + "\t\t" + Rep + "\t\t" + NProc + "\t\t" + sub/1000 + "\n"
					
							var writer = new FileWriter(new File("../../docs/scala/" + FileName))
							writer.write(lines);
							writer.close();
						}
						catch {
					 		case _ => {
								var writer = new FileWriter(new File("../../docs/scala/" + FileName))
						 		var lines = "DataSize\tRepetition\tNProc\tTime[us]\n"	
						 		lines = lines + DataSize + "\t\t" + Rep + "\t\t" + NProc + "\t\t" + sub/1000 + "\n"
						 		writer.write(lines);
						 		writer.close();	
					 		}
						}	 	
						exit('stop)
					}
				}
			}
		}
	}
}

class ProcInitial(var Data: Array[Byte], var Rep: Int, var Finalize: Actor) extends Actor {
	def act(){
		var MailBox: Array[Byte] = null
		var Peer: Actor = null
		loop{
			react{
				case value: Proc => {
					Peer = value
					Peer ! Data
				}
				case value: Array[Byte] => {
					MailBox = Data.clone();
					Rep = Rep -1
					if(Rep == 0) {
						Finalize ! 1
						exit('stop)
					}
					else {
						Peer ! Data			
					}
				}
			}
		}
	}
}

class Proc(var Data: Array[Byte], var Rep: Int, var Peer: Actor, var Finalize: Actor) extends Actor {
	def act(){
		var MailBox: Array[Byte] = null	
		loop{
			react{
				case value: Array[Byte] => {
					MailBox = Data.clone();
					Rep = Rep -1		 
					Peer ! Data
					if(Rep == 0)
					{
						Finalize ! 1
						exit('proc)
					}
				}
			}
		}
	}
}
