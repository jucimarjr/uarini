#!/usr/bin/env ruby

class Node

	def initialize(nextNode, data, boolean)
		@nextNode = nextNode
		@data = data
		@boolean = boolean
		@mailBox = Array.new
	end

	def connect(nextNode, data, boolean)
		@nextNode = nextNode
		@data = data
		@boolean = boolean
	end
			
	def send(dado)
		@mailBox = dado
	end

	def recv()
		while (true)		
			if (@mailBox != nil) and (@mailBox.length == @data.length)				
				@mailBox= Array.new
				break
			end 
		end
	end

	def start
		while (true)
			recv()
			print("#{self} recebeu\n")
			@nextNode.send(@data);				
		end
	end
end

class SendRecv < Node	
	def initialize(tamMsg, qtdMsg, qtdProc)
		super(nil, nil , true)		
		@tamMsg = tamMsg
		@qtdMsg = qtdMsg
		@qtdProc = qtdProc
		@nodes = Array.new(@qtdProc, nil)
		@threads = Array.new(@qtdProc, nil)
	end

	def spawnAndConnectNodes(array)		
		n = @qtdProc

		@nodes[0] = self		
		@nodes[n-1] = Node.new(@nodes[0], array, true)		
		@threads[n-1] = Thread.new { @nodes[n-1].start }
 
		index = n - 2
		while (index > 0)
			@nodes[index] = Node.new(@nodes[index+1], array, true)
			@threads[index] = Thread.new { @nodes[index].start }
			print("#{@nodes[index]} #{@nodes[index + 1]}\n")
			index = index - 1
		end
		@nodes[0].connect(@nodes[1], array, true)		
	end

	def senderNodeMode(msg)
		index = 1
		while (index <= @qtdMsg):
			@nextNode.send(msg)
			recv()
			print("#{self} recebeu\n")
			index = index + 1
		end
	end

	def start
		t = Thread.new do
				
				array = Array.new(@tamMsg, 1)
				spawnAndConnectNodes(array)
		
				time1 = Time.now
				senderNodeMode(array)
				time2 = Time.now
				delta = time2 - time1

				line = "#{@tamMsg}\t#{@qtdMsg}\t#{"%.6f" % delta.to_f}"			
				
				if File.exists?("temp.txt")
					file = File.open("temp.txt", "a")	
					file.puts(line)
					file.close 			
				else
					file = File.open("temp.txt", "w")
					file.puts(line)
					file.close
				end
		end
		t.join
	end
end

if __FILE__ == $0
	
	tamMsg = ARGV[0].to_i
	qtdMsg = ARGV[1].to_i
	qtdProc = ARGV[2].to_i
	
	sendRecv = SendRecv.new(tamMsg, qtdMsg, qtdProc)
	sendRecv.start
end
