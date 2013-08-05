import actors.Actor
import actors.Actor._
import java.io._
import scala.io.Source

object sendrecv{
 
  def main(args: Array[String]){
    
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

    var ArrayProc = new Array[Proc](NProc)       
    var Finalize = new Finalize(DataSize, Rep, NProc, FileName, ArrayProc)
    
    i = 0
    while(i < NProc)
    {
      ArrayProc(i) = new Proc(Data, Rep, Finalize)
      ArrayProc(i).start()
      i = i + 1;
    }
    
    Finalize.start    
    i = 0
    while(i < NProc - 1)
    {
      ArrayProc(i) ! ArrayProc(i+1)
      i = i + 1;
    }
    ArrayProc(i) ! ArrayProc(0)
  }
}

class Finalize(var DataSize: Int, var Rep: Int, var NProc: Int, var FileName: String, ArrayProc: Array[Proc]) extends Actor{  
	def act(){		
		var start = System.nanoTime()
		var N : Long = NProc
		var finish: Long = 0
		var sub: Long = 0
		loop{
	      react{
	        case 1 =>{
	          N = N - 1
   		      finish = System.nanoTime()	          
	          sub = sub + finish - start;
   		      if(N == 0)
	          {
		          sub = sub/NProc		          
	              var i = 0
	              while(i < ArrayProc.length)
	              {
	                ArrayProc(i) ! 1
	                i = i + 1
	              }
     		      
		          try{
		            var source = scala.io.Source.fromFile("../../docs/scala/" + FileName)
		          	var lines = source .mkString
		          	lines = lines + DataSize + "\t\t" + Rep + "\t\t" + NProc + "\t\t" + sub/1000 + "\n"
		          	
					var writer = new FileWriter(new File("../../docs/scala/" + FileName))
			        writer.write(lines);
			        writer.close();
		          }
		          catch  {
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

class Proc(var Data: Array[Byte], var Rep: Int, var Finalize: Actor) extends Actor{
  def act(){
    var MailBox: Array[Byte] = null
    var Peer: Proc = null    
    loop{
      react{
        case value: Proc =>{          
          Peer = value
          Peer ! (self, Data)
        }
        case value: (Proc, Array[Byte]) =>{                    
          MailBox = Data.clone();
          var proc = value._1;
          if(proc == self){
            Rep = Rep -1
          }
          if(Rep == 0){
            Finalize ! 1
          }
          else{
            Peer ! (proc, Data)            
          }
        }
        case 1 => {
          exit('stop)
        }
      }
    }   
  }
}
