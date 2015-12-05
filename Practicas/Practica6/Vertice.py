from Arista import Arista

class Vertice:
	elemento=''
	adyacentes= [] 
	
	def __init__(self, elemento):
		self.elemento = elemento
			
	def getElemento(self):
		return self.elemento
		
	def setElemento(self):
		self.elemento = elemento

	def creaArista(self, origen, destino):
		arista = Arista(origen, destino)
		self.adyacentes.append(arista.getArista())
		
	def degree(self):
		return len(self.adyacentes)

	def neighbours(self):
		return self.adyacentes

#vA = Vertice('a')
#vA.creaArista(vB.getElemento())
