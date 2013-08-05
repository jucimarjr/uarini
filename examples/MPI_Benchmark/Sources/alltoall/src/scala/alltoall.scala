import actors.Actor
import actors.Actor._
import java.io._
import scala.io.Source

object alltoall{
 
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
     
    var Finalize = new Finalize(DataSize, Rep, NProc, FileName)    
    
    var ArrayProc = new Array[Proc](NProc)    
    
    i = 0    
    while(i < NProc)
    {
      ArrayProc(i) = new Proc(i, Data, Rep, Finalize)
      ArrayProc(i).start  
      i = i + 1;
    }

    Finalize.start
    i = 0    
    while(i < NProc)
    {
      ArrayProc(i) ! ArrayProc
      i = i + 1;
    }
   }
}

class Finalize(var DataSize: Int, var Rep: Int, var NProc: Int, var FileName: String) extends Actor{  
	def act(){		
		var start = System.nanoTime()
		var N : Int = NProc
		loop{
	      react{
	        case 1 =>{
	          N = N - 1
	          if(N == 0)
	          {
		          var finish = System.nanoTime
		          var sub = finish - start		          
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

class Proc(var Id: Int, var Data: Array[Byte], var Rep: Int, var Finalize: Actor) extends Actor{
def act(){
    var MailBox: Array[Byte] = null
    var total = -1
    var j = 0
    var i = 0
    loop{
      react{        
        case value: Array[Proc]=> {                   	
            total = value.length * Rep
            j = 0
            while(j < Rep){
	          i = 0	        
	          while(i < value.length){
	             value(i)! Data
	             i = i + 1	        	
	          }
              j = j + 1         
	        }
        }
      	case value: Array[Byte] =>{      	            
          MailBox = Data.clone();                  
          total = total - 1
          if(total == 0)
          {
            Finalize ! 1
            exit('proc)
          }          
        }
      }
    }   
  }
}
