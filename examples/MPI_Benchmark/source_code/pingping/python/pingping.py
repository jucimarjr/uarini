#!/usr/bin/env python
# -*- coding: latin-1 -*-

import sys
import datetime
from threading import Thread

class ProcPing(Thread):
	def __init__(self, name, data, qtdMsg):
		Thread.__init__(self)							
		self.name = name
		self.data = data
		self.qtdMsg = qtdMsg
		self.mailBox = []

	def setPeer(self, Peer):
		self.Peer = Peer

	def send(self, dado):		
		self.Peer.mailBox = dado

	def recv(self):
		while True:	
			if not len(self.mailBox) < len(self.data):								
				print(self)				
				self.mailBox = []
				break

	def run(self):
		for i in range (0, self.qtdMsg + 1):
			self.send(self.data)
			if i < self.qtdMsg:
				self.recv()

class PingPing(Thread):

	def __init__(self, tamMsg, qtdMsg):
		Thread.__init__(self)
		self.tamMsg = tamMsg
		self.qtdMsg = qtdMsg

	def run(self):
		index = 0
		array = [1]			
		while index < self.tamMsg -1:
			array.append(1)
			index = index + 1
		
		p1 = ProcPing("1", array, self.qtdMsg)
		p2 = ProcPing("2", array, self.qtdMsg)
		
		p2.setPeer(p1)
		p1.setPeer(p2)
		
		timeStart = datetime.datetime.now()
		p1.start()		
		p2.start()
		
		p1.join()
		p2.join()
		timeEnd = datetime.datetime.now()
		
		timeExec = timeEnd - timeStart

		line = "%d\t%d\t%s\n" % (self.tamMsg, self.qtdMsg, timeExec)
	
		try:
			arq = open('saida.txt', 'r')
			textoSaida = arq.read()
			arq.close()
		except:
			arq = open('saida.txt', 'w')
			textoSaida = ""
			arq.close()

		arq = open('saida.txt', 'w')
		textoSaida = textoSaida + line
		arq.write(textoSaida)
		arq.close()

def main():
	param = sys.argv[1:]

	tamMsg = int(param[0])
	qtdMsg = int(param[1])

	pingPing = PingPing(tamMsg, qtdMsg)
	pingPing.start()

if __name__=="__main__":
	main()
