#!/usr/bin/env ruby

class ProcPing

	def initialize(name, data, qtdMsg)
		@name = name
		@data = data
		@qtdMsg = qtdMsg
		@mailBox = Array.new
		@i = -1
	end

	def setPeer(peer)
		@peer = peer
	end
	
	def setMailBox(data)
		@mailBox = data.clone
	end
			
	def send()
		@peer.setMailBox(@data)
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
			send()	
			if(@i < @qtdMsg - 1)
				recv()				
			else
				break	
			end
			@i = @i + 1
		end
	end
end

class ProcPong

	def initialize(name, data, qtdMsg)
		@name = name
		@data = data
		@qtdMsg = qtdMsg
		@mailBox = Array.new
		@i = -1
	end

	def setPeer(peer)
		@peer = peer
	end
	
	def setMailBox(data)
		@mailBox = data.clone
	end
			
	def send()
		@peer.setMailBox(@data)
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
			if(@i < @qtdMsg - 1)
				send()					
			else
				break	
			end
			@i = @i + 1
		end
	end
end

class PingPong	
	def initialize(tamMsg, qtdMsg)
		@tamMsg = tamMsg
		@qtdMsg = qtdMsg
	end

	def start
		t = Thread.new do
			array = Array.new(@tamMsg, 1)

			p1 = ProcPing.new("1", array, @qtdMsg)
			p2 = ProcPong.new("2", array, @qtdMsg)
			
			p2.setPeer(p1)
			p1.setPeer(p2)
	
			time1 = Time.now

			t1 = Thread.new { p1.start }			
			t2 = Thread.new { p2.start }
			
			t1.join	
				
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
	
	pingPing = PingPong.new(tamMsg, qtdMsg)
	pingPing.start
end
