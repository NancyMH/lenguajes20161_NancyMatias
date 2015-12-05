from xml.dom import minidom
import csv
import json 
from lxml import etree


class GraphReader:
	archivo =''
	vertices=[]
	aristas=[]
	direct=0

	def __init__(self, entrada):
		self.tipoArchivo(entrada)

	def tipoArchivo(self, entrada):
		if 'xml' in entrada:
			return self.tipoXML(entrada)
		elif 'json' in entrada:
			return self.tipoJSON(entrada)
		elif 'csv'in entrada:
			return self.tipoCSV(entrada)
		return "Desconocido"

	def tipoXML(self, entrada):
		doc = etree.parse(entrada)
		raiz=doc.getroot()
		atributos = raiz.attrib
		dirigida = atributos.get('direct')
		self. archivo = minidom.parse(entrada)
		vertexs = self.archivo.getElementsByTagName("vertex")
		
		for vertex in vertexs:
			vertice = vertex.getAttribute("label")
			self.vertices.append(vertice)
		self.vertices[2]


		for edge in raiz.findall('edge'):
			inicio = edge.get('source')
			destino = edge.get('target')
			peso = edge.get('weight')
			self.aristas.append((inicio,destino,peso))


	
	def tipoCSV(self, entrada):
					
		for linea in lineas():
			if ('direct=1' == linea[0]):
				self.direct= 1
			vertice = linea[0]
			self.vertices.append(vertice)
			destino = linea[1]
			peso = linea [2]
			self.aristas.append((vertice,destino,peso))
		self.archivo.close()



	def tipoJSON(self, entrada):
		
		with open(entrada) as archivo:
			datos = json.load(archivo)
		print(datos)

		

	def getVertices(self):
		return self.vertices
	def getAristas(self):
		return self.aristas
	def getDirect(self):
		return self.direct


print("***GRAFICA XML***")
g = GraphReader("graph.xml")
print (g.getDirect())
print (g.getVertices())
print (g.getAristas())


print("***GRAFICA JSON***")
g1 = GraphReader("graph.json")
print (g1.getDirect())

g2 = GraphReader("graph.csv")
print (g2.getDirect())
print (g2.getVertices())
print (g2.getAristas())
