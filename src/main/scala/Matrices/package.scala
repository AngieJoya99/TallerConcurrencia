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
      * @return
      */
    def multMatrizPar(m1:Matriz, m2:Matriz): Matriz ={
      val m2Transpuesta = transpuesta(m2)
      Vector.tabulate(m1.size,m1.size)((i,j) => prodPunto(m1(i), m2Transpuesta(j)))
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
      Vector(Vector(0))
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices en paralelo usando recursión
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    def multMatrizRecPar(m1:Matriz, m2:Matriz): Matriz ={
      Vector(Vector(0))
    }

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
      Vector(Vector(0))
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación en paralelo de las dos matrices usando el 
      * algortimo de Strassen
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    def multStrassenPar(m1:Matriz, m2:Matriz): Matriz ={
      Vector(Vector(0))
    }  
}

