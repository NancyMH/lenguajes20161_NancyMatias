class Arista:
	origen=''
	destino=''

	def __init__(self, origen, destino):
		self.destino = destino
		self.origen=origen

	def svertex(self):
		return self.origen

	def tvertex(self):
		return self.destino
