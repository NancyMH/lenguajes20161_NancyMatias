from Arista import Arista

class AristaPeso(Arista):
	peso=0

	def __init__(self, origen, destino, peso):
		Arista.__init__(self, origen, destino)
		self.peso = peso

	def weight(self):
		return self.peso
