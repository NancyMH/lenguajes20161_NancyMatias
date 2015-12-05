class Graph:
	archivo =''
	tipo=0
	def __init__(self, ruta, tipo):
		self.archivo = ruta
		self.tipo = tipo
		self.directed(tipo)

		#self.leer(ruta)
	def leer(self, ruta):
		self.archivo = open(ruta, "r") 
		for linea in self.archivo.readlines():
			print linea
		self.archivo.close()

	def directed(self, tipo):
		if self.tipo == 1:
			return True
		elif self.tipo == 0:
			return False
		return "Desconocido"


g = Graph("grafica.xml", 5)
print g.toString;