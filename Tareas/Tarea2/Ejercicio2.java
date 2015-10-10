public class Ejercicio2{
	public static void main(String [] args){
		double ejemploPerezoso = 100 + perezosa(-78);
		double ejemploGloton = 100-78;
		System.out.println("Este es el ejemplo de evaluación perezosa :"+ ejemploPerezoso);
		System.out.println("Este es el ejemplo de evaluación glotona :"+ ejemploGloton);
	}
	public static double perezosa(int num){
		return  Math.log(num);
	}
}