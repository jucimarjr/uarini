#!/usr/bin/env python
# -*- coding: latin-1 -*-

import sys
import datetime
from threading import Thread
from datetime import timedelta

class Message():
	def __init__(self, proc, data):
		self.proc = proc
		self.data = data

class Node(Thread):
	def __init__(self, rep, data, parent, boolean):
		Thread.__init__(self)							
		self.rep = rep
		self.data = data
		self.parent = parent 
		self.mailBox = []
		self.boolean = boolean

	def setRightPeer(self, rightPeer):
		self.rightPeer = rightPeer

	def send(self, dado):		
		self.rightPeer.mailBox.append(dado)

	def recv(self):
		while True:							
			if self.boolean == False:	
				break			 
			if len(self.mailBox) != 0:												
				return self.mailBox[0]						

	def run(self):
		m = Message(self, self.data)
		self.send(m)
		while True:
			if self.boolean == False:	
				break
			msg = self.recv()
			
			if len(self.mailBox) > 0:
				del self.mailBox[0]
			
			if msg == m:
				self.rep = self.rep - 1				

			if self.rep == 0:				
				self.parent.send([1])
			else:		
				self.send(msg)

class SendRecv(Thread):

	def __init__(self, tamMsg, qtdMsg, qtdProc):
		Thread.__init__(self)
		self.tamMsg = tamMsg
		self.qtdMsg = qtdMsg
		self.qtdProc = qtdProc
		self.nodes = []
		self.mailBox = []
		self.count = 0

	def send(self, dado):		
		self.mailBox.append(dado)

	def recv(self):
		while True:	
			if len(self.mailBox) != 0:							
				del self.mailBox[0]
				break

	def stop(self):
		n = self.qtdProc
  		index = 0		
		while index < n:
			self.nodes[index].boolean = False
			index = index + 1

	def run(self):
		endTimeList = []
		timeList = []

		index = 0 
		array = [1]			
		while index < self.tamMsg -1:
			array.append(1)
			index = index + 1

		index = 0
		while(index < self.qtdProc):
			self.nodes.append(Node(self.qtdMsg, array, self, True))
			index = index + 1

		index = 0
		while(index < self.qtdProc -1):
			self.nodes[index].setRightPeer(self.nodes[index + 1])
			index = index + 1
		self.nodes[self.qtdProc -1].setRightPeer(self.nodes[0])

		timeStart = datetime.datetime.now()
		
		index = 0
		while(index < self.qtdProc):
			self.nodes[index].start()
			index = index + 1
		
		while(self.count != self.qtdProc):
			self.recv()				
			endTimeList.append(datetime.datetime.now())	
			self.count = self.count + 1

		index = 0		

		timeExec = self.avg(endTimeList, timeStart)  		
		line = "%d\t%d\t%s\t%s\n" % (self.tamMsg, self.qtdMsg, self.qtdProc, timeExec)

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
		self.stop()
	
	def avg(self, a, t):
	    sumdeltas = timedelta(seconds=0)

	    i = 0
	    while i < len(a):
		delta = abs(a[i] - t)
		try:
		    sumdeltas += delta
		except:
		    raise
		i += 1
	    avg = sumdeltas / len(a)
	    return avg

def main():
	param = sys.argv[1:]

	tamMsg = int(param[0])
	qtdMsg = int(param[1])
	qtdProc = int(param[2])

	sendRecv = SendRecv(tamMsg, qtdMsg, qtdProc)
	sendRecv.start()

if __name__=="__main__":
	main()
