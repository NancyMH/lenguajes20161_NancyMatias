class Vertice:
	elemento=''
	adyacentes= [] 
	
	def __init__(self, elemento):
		self.elemento = elemento
			
	def getElemento(self):
		return self.elemento
		
	def setElemento(self):
		self.elemento = elemento

	def creaVecino(self, origen, destino):
		self.adyacentes.append((origen, destino))
		
	def degree(self):
		return len(self.adyacentes)

	def neighbours(self):
		return self.adyacentes
