#!/usr/bin/env python
# -*- coding: latin-1 -*-

import sys
import datetime
from threading import Thread

class Node(Thread):
	def __init__(self, nextNode, data, boolean):
		Thread.__init__(self)							
		self.nextNode = nextNode
		self.data = data 
		self.mailBox = []
		self.boolean = boolean

	def connect(self, nextNode, data, boolean):
		self.nextNode = nextNode
		self.data = data
		self.boolean = boolean
	
	def send(self, dado):		
		self.mailBox = dado

	def recv(self):
		while True:	
			if self.boolean == False:	
				break
			if not len(self.mailBox) < len(self.data):							
				self.mailBox = []
				break

	def run(self):
		while True:
			if self.boolean == False:	
				break
			self.recv()
			self.nextNode.send(self.data)

class ThreadRing(Node):

	def __init__(self, tamMsg, qtdMsg, qtdProc):
		Node.__init__(self, None, [], True)
		self.tamMsg = tamMsg
		self.qtdMsg = qtdMsg
		self.qtdProc = qtdProc
		self.nodes = []

	def spawnAndConnectNodes(self, array):
		n = self.qtdProc
		
		self.nodes = [self]
		index = 1		
		while index < n:
			self.nodes.append(None)
			index = index + 1

		self.nodes[n - 1] = Node(self.nodes[0], array, True)
		self.nodes[n - 1].start()

		index = n - 2
	
		while index > 0:
			self.nodes[index] = Node(self.nodes[index + 1], array, True)
			self.nodes[index].start()
			index = index - 1

		self.nodes[0].connect(self.nodes[1], array, True)

	def stop(self):
		n = self.qtdProc
  		index = 0		
		while index < n:
			self.nodes[index].boolean = False
			index = index + 1

	def senderNodeMode(self, msg):
		index = 1
		while (index <= self.qtdMsg):
			self.nextNode.send(msg)
			self.recv()
			index = index + 1

	def run(self):
		index = 0 
		array = [1]			
		while index < self.tamMsg -1:
			array.append(1)
			index = index + 1

		self.spawnAndConnectNodes(array)

		timeStart = datetime.datetime.now()
		self.senderNodeMode(array)
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
		self.stop()

def main():
	param = sys.argv[1:]

	tamMsg = int(param[0])
	qtdMsg = int(param[1])
	qtdProc = int(param[2])

	threadRing = ThreadRing(tamMsg, qtdMsg, qtdProc)
	threadRing.start()

if __name__=="__main__":
	main()
