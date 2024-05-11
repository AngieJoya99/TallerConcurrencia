/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

import common._
import scala.util.Random
import scala.collection.parallel.immutable.ParVector
package object Matrices {
    val random = new Random()
    type Matriz = Vector[Vector[Int]]

    /**
      * Crea una matriz cuadrada de dimensiones long x long
      * con valores enteros aleatorios entre 0 y vals
      * @param long dimensiones de la matriz
      * @param vals valor máximo (abierto) del aleatorio
      * @return matriz cuadrada
      */
    def matrizAlAzar (long:Int, vals:Int): Matriz ={
      val v = Vector.fill(long,long){random.nextInt(vals)}
      v
    }

    /**
      * Crea un vector de tamaño longs y valores enteros aleatorios
      * entre 0 y vals
      * @param long longitud del vector
      * @param vals valor máximo (abierto) del aleatorio
      * @return vector de enteros
      */
    def vectorAlAzar (long:Int, vals:Int): Vector[Int] ={
      val v = Vector.fill(long){random.nextInt(vals)}
      v
    }

    /**
      * Traspone una matriz m
      * @param m matriz
      * @return matriz transpuesta
      */
    def transpuesta(m:Matriz): Matriz ={
      val l = m.length
      Vector.tabulate(l,l)((i,j) => m(j)(i))
    }

    /**
      * Calcula el producto punto entre dos vectores v1 y v2
      * @param v1 vector 1
      * @param v2 vector 2
      * @return producto punto entre v1 y v2
      */
    def prodPunto(v1:Vector[Int], v2:Vector[Int]): Int ={
      (v1 zip v2).map({case (i,j)=> (i*j)}).sum
    }

    /**
      * Calcula de manera paralela el producto punto entre dos vectores v1 y v2
      * @param v1 Vector paralelo 1
      * @param v2 Vector paralelo 2
      * @return producto punto entre v1 y v2
      */
    def prodPuntoParD(v1:ParVector[Int], v2:ParVector[Int]): Int ={
      (v1 zip v2).map({case (i,j)=> (i*j)}).sum
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices 
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return
      */
    def multMatriz(m1:Matriz, m2:Matriz): Matriz ={
      val m2Transpuesta = transpuesta(m2)
      Vector.tabulate(m1.size,m1.size)((i,j) => prodPunto(m1(i), m2Transpuesta(j)))
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices en paralelo
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Matriz resultante de m1 x m2
      */
    def multMatrizPar(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length<=1)(multMatriz(m1,m2))
      else{
        val m2Transpuesta = transpuesta(m2)
        val mPar = Vector.tabulate(m1.size,m1.size)((i,j) => task(prodPunto(m1(i), m2Transpuesta(j))))
        mPar.map(x => x.map(y => y.join()))
      }      
    }

    /**
      * Dada una matriz cuadrada m de tamaño NxN, 1<=i, j<=N, i+n<=N, j+n<=N,
      * Retorna la submatriz de NxN correspondiente a m[i..i+(n-1) , j..j+(n-1)]
      * @param m Matriz
      * @param i tamaño de x
      * @param j tamaño de y
      * @param l longitud de la matriz
      * @return Submatriz
      */
    def subMatriz(m:Matriz, i:Int, j:Int, l:Int): Matriz ={
      i match {
        case 0 => j match {
          case 0 => Vector.tabulate(l,l)((i,j) => m(i)(j))
          case l => Vector.tabulate(l,l)((i,j) => m(i)(l+j))
          }
        case l => j match {
          case 0 => Vector.tabulate(l,l)((i,j) => m(i+l)(j))
          case l => Vector.tabulate(l,l)((i,j) => m(l+i)(l+j))
        }
      }
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la suma de las dos matrices
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Suma de m1 y m2
      */
    def sumMatriz(m1:Matriz, m2:Matriz): Matriz ={
      val l = m1.length
      Vector.tabulate(l,l)((i,j) => m1(i)(j)+m2(i)(j))
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices usando recursión
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    def multMatrizRec(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length ==1) (Vector(Vector(m1(0)(0) * m2(0)(0))))
      else{
        val mitad = m1.length/2
        val subM1 = Vector(subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val subM2 = Vector(subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        val subMultIzq = Vector(multMatrizRec(subM1(0),subM2(0)),multMatrizRec(subM1(0),subM2(1)),multMatrizRec(subM1(2),subM2(0)),multMatrizRec(subM1(2),subM2(1)))
        val subMultDer = Vector(multMatrizRec(subM1(1),subM2(2)),multMatrizRec(subM1(1),subM2(3)),multMatrizRec(subM1(3),subM2(2)),multMatrizRec(subM1(3),subM2(3)))
        val mult = Vector(sumMatriz(subMultIzq(0),subMultDer(0)),sumMatriz(subMultIzq(1),subMultDer(1)),sumMatriz(subMultIzq(2),subMultDer(2)),sumMatriz(subMultIzq(3),subMultDer(3)))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (mult(0)(i)(j))
          else if (i < mitad && j >= mitad) (mult(1)(i)(j-mitad))
          else if(i >= mitad && j < mitad) (mult(2)(i-mitad)(j))
          else (mult(3)(i-mitad)(j-mitad)))
      }      
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices en paralelo usando recursión
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    /*def multMatrizRecPar(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length <= math.pow(2,0)) (multMatrizRec(m1,m2))
      else{
        val mitad = m1.length/2
        val subM1 = Vector(subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val subM2 = Vector(subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        val subMultIzq = Vector(task(multMatrizRec(subM1(0),subM2(0))),task(multMatrizRec(subM1(0),subM2(1))),task(multMatrizRec(subM1(2),subM2(0))),task(multMatrizRec(subM1(2),subM2(1))))
        val subMultDer = Vector(task(multMatrizRec(subM1(1),subM2(2))),task(multMatrizRec(subM1(1),subM2(3))),task(multMatrizRec(subM1(3),subM2(2))),task(multMatrizRec(subM1(3),subM2(3))))
        subMultIzq.map(x => x.join())
        subMultDer.map(x => x.join())
        val mult = Vector(parallel(sumMatriz(subMultIzq(0),subMultDer(0)),sumMatriz(subMultIzq(1),subMultDer(1)),sumMatriz(subMultIzq(2),subMultDer(2)),sumMatriz(subMultIzq(3),subMultDer(3))))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (mult(0)(i)(j))
          else if (i < mitad && j >= mitad) (mult(1)(i)(j-mitad))
          else if(i >= mitad && j < mitad) (mult(2)(i-mitad)(j))
          else (mult(3)(i-mitad)(j-mitad)))
      }      
    }*/
    

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la resta de las dos matrices
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Resta de m1 y m2
      */
    def restaMatriz(m1:Matriz, m2:Matriz): Matriz ={
      val l = m1.length
      Vector.tabulate(l,l)((i,j) => m1(i)(j) - m2(i)(j))
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices usando el algortimo de Strassen
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    def multStrassen(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length ==1) (Vector(Vector(m1(0)(0) * m2(0)(0))))
      else{
        val mitad = m1.length/2
        val subM1 = Vector(subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val subM2 = Vector(subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        val mS1 = Vector(restaMatriz(subM2(1),subM2(3)),sumMatriz(subM1(0),subM1(1)),sumMatriz(subM1(2),subM1(3)),restaMatriz(subM2(2),subM2(0)),sumMatriz(subM1(0),subM1(3)))
        val mS2 = Vector(sumMatriz(subM2(0),subM2(3)),restaMatriz(subM1(1),subM1(3)),sumMatriz(subM2(2),subM2(3)),restaMatriz(subM1(0),subM1(2)),sumMatriz(subM2(0),subM2(1)))
        val mP = Vector(multStrassen(subM1(0),mS1(0)),multStrassen(mS1(1),subM2(3)),multStrassen(mS1(2),subM2(0)),multStrassen(subM1(3),mS1(3)),multStrassen(mS1(4),mS2(0)),multStrassen(mS2(1),mS2(2)),multStrassen(mS2(3),mS2(4)))
        val mult = Vector(sumMatriz(restaMatriz(sumMatriz(multStrassen(mS1(4),mS2(0)),multStrassen(subM1(3),mS1(3))),multStrassen(mS1(1),subM2(3))),multStrassen(mS2(1),mS2(2))),sumMatriz(multStrassen(subM1(0),mS1(0)),multStrassen(mS1(1),subM2(3))),sumMatriz(multStrassen(mS1(2),subM2(0)),multStrassen(subM1(3),mS1(3))),restaMatriz(restaMatriz(sumMatriz(multStrassen(mS1(4),mS2(0)),multStrassen(subM1(0),mS1(0))),multStrassen(mS1(2),subM2(0))),multStrassen(mS2(3),mS2(4))))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (mult(0)(i)(j))
          else if (i < mitad && j >= mitad) (mult(1)(i)(j-mitad))
          else if(i >= mitad && j < mitad) (mult(2)(i-mitad)(j))
          else (mult(3)(i-mitad)(j-mitad)))
      }
    }

    
    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación en paralelo de las dos matrices usando el 
      * algortimo de Strassen
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    /*def multStrassenPar(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length <=1) (multStrassen(m1,m2))
      else{
        val mitad = m1.length/2
        val subM1 = Vector(parallel(subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad)))
        val subM2 = Vector(parallel(subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad)))
        val mS1 = Vector(restaMatriz(subM2(1),subM2(3)),sumMatriz(subM1(0),subM1(1)),sumMatriz(subM1(2),subM1(3)),restaMatriz(subM2(2),subM2(0)),sumMatriz(subM1(0),subM1(3)))
        val mS2 = Vector(sumMatriz(subM2(0),subM2(3)),restaMatriz(subM1(1),subM1(3)),sumMatriz(subM2(2),subM2(3)),restaMatriz(subM1(0),subM1(2)),sumMatriz(subM2(0),subM2(1)))
        val mP = Vector(multStrassen(subM1(0),mS1(0)),multStrassen(mS1(1),subM2(3)),multStrassen(mS1(2),subM2(0)),multStrassen(subM1(3),mS1(3)),multStrassen(mS1(4),mS2(0)),multStrassen(mS2(1),mS2(2)),multStrassen(mS2(3),mS2(4)))
        val mult = Vector(parallel(sumMatriz(restaMatriz(sumMatriz(multStrassen(mS1(4),mS2(0)),multStrassen(subM1(3),mS1(3))),multStrassen(mS1(1),subM2(3))),multStrassen(mS2(1),mS2(2))),sumMatriz(multStrassen(subM1(0),mS1(0)),multStrassen(mS1(1),subM2(3))),sumMatriz(multStrassen(mS1(2),subM2(0)),multStrassen(subM1(3),mS1(3))),restaMatriz(restaMatriz(sumMatriz(multStrassen(mS1(4),mS2(0)),multStrassen(subM1(0),mS1(0))),multStrassen(mS1(2),subM2(0))),multStrassen(mS2(3),mS2(4)))))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (mult(0)(i)(j))
          else if (i < mitad && j >= mitad) (mult(1)(i)(j-mitad))
          else if(i >= mitad && j < mitad) (mult(2)(i-mitad)(j))
          else (mult(3)(i-mitad)(j-mitad)))
      }
    }*/
}

object Main {
  def main(args: Array[String]): Unit = {
    import Matrices._
    import Benchmark._

    //val m1 = matrizAlAzar(math.pow(2,2).toInt,2)
    //val m2 = matrizAlAzar(math.pow(2,2).toInt,2)
    
    val m1 = Vector(Vector(1, 1, 0, 1), Vector(0, 1, 0, 1), Vector(1, 1, 0, 0), Vector(0, 0, 1, 1))
    val m2 = Vector(Vector(0, 1, 0, 0), Vector(0, 1, 0, 1), Vector(0, 0, 1, 0), Vector(1, 0, 0, 0))

    //Método Estándar
    println(multMatriz(m1,m2))
    println(multMatriz2(m1,m2))
    //println(multMatrizPar(m1,m2))
    //println(multMatrizRec(m1,m2))
    //println(multStrassen(m1,m2))

    //Método Recursivo
    //println(multMatrizRec(m1,m2))
  }
}


