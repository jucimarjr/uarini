import actors.Actor
import actors.Actor._
import java.io._
import scala.io.Source

object pingping{
 
	def main(args: Array[String]){
	
		var DataSize : Int = Integer.parseInt(args(0))
		var Rep : Int = Integer.parseInt(args(1))	
		var FileName = args(2)	
		var Data = new Array[Byte](DataSize)	
		var i = 0;
		while(i < DataSize)
		{
			Data(i) = 1;
			i = i +1;
		}

		var finalize = new Finalize(DataSize, Rep, FileName)
		var ping1 = new Ping(Data, Rep, finalize) 
		var ping2 = new Ping(Data, Rep, finalize)

		finalize.start
		ping1.start
		ping2.start
	
		ping1 ! ping2
		ping2 ! ping1
	}
}

class Finalize(DataSize: Int, Rep: Int, FileName: String) extends Actor{
	def act(){
		var total = 0
		var start = System.nanoTime()
		loop{
			react{
				case 1 => {
					total = total + 1;
					if(total == 2) {
						var finish = System.nanoTime()
						var sub = finish - start
						try {
					 		var source = scala.io.Source.fromFile("../../docs/scala/" + FileName)
							var lines = source .mkString
							lines = lines + DataSize + "\t\t" + Rep + "\t\t" + sub/1000 + "\n"
						 
							var writer = new FileWriter(new File("../../docs/scala/" + FileName))
							writer.write(lines);
							writer.close();
						}
						catch {
							case _ => {
								var writer = new FileWriter(new File("../../docs/scala/" + FileName))
						 		var lines = "DataSize\tRepetition\tTime[us]\n"	
						 		lines = lines + DataSize + "\t\t" + Rep + "\t\t" + sub/1000 + "\n";
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


class Ping(Data: Array[Byte], var Rep: Int, Main: Actor) extends Actor{	
	def act(){
		var MailBox : Array[Byte] = null
		loop{
			react{
				case value: Ping => {
					if(Rep > 0) {
						MailBox = Data.clone()
						value ! MailBox
						Rep = Rep -1
					} else{
						Main ! 1
						exit('stop)
					}
				}
				case value: Array[Byte] => {
					MailBox = Data.clone()
					if(Rep > 0) {
						sender ! MailBox
						Rep = Rep -1
					}
					else {
						Main ! 1
						exit('stop)
					} 
				}
			}
		}
	}
}
