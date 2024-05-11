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
        if (m1.length<= math.pow(2,0))(multMatriz(m1,m2))
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
        val (a11, a12, a21, a22) = (subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val (b11, b12, b21, b22) = (subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        val (ci1, ci2, ci3, ci4) = (multMatrizRec(a11,b11),multMatrizRec(a11,b12),multMatrizRec(a21,b11),multMatrizRec(a21,b12))
        val (cd1, cd2, cd3, cd4) = (multMatrizRec(a12,b21),multMatrizRec(a12,b22),multMatrizRec(a22,b21),multMatrizRec(a22,b22))
        val (c1, c2, c3, c4) = (sumMatriz(ci1,cd1),sumMatriz(ci2,cd2),sumMatriz(ci3,cd3),sumMatriz(ci4,cd4))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (c1(i)(j))
          else if (i < mitad && j >= mitad) (c2(i)(j-mitad))
          else if(i >= mitad && j < mitad) (c3(i-mitad)(j))
          else (c4(i-mitad)(j-mitad)))
      }      
    }

    /**
      * Dadas dos matrices cuadradas del mismo tamaño (potencia de 2) m1 y m2,
      * calcula la multiplicación de las dos matrices en paralelo usando recursión
      * @param m1 Matriz 1
      * @param m2 Matriz 2
      * @return Multiplicación de m1 y m2
      */
    def multMatrizRecPar(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length <= math.pow(2,0)) (multMatrizRec(m1,m2))
      else{
        val mitad = m1.length/2
        val (a11, a12, a21, a22) = parallel(subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val (b11, b12, b21, b22) = parallel(subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        val (ci1, ci2, ci3, ci4) = parallel(multMatrizRecPar(a11,b11),multMatrizRecPar(a11,b12),multMatrizRecPar(a21,b11),multMatrizRecPar(a21,b12))
        val (cd1, cd2, cd3, cd4) = parallel(multMatrizRecPar(a12,b21),multMatrizRecPar(a12,b22),multMatrizRecPar(a22,b21),multMatrizRecPar(a22,b22))
        val (c1, c2, c3, c4) = parallel(sumMatriz(ci1,cd1),sumMatriz(ci2,cd2),sumMatriz(ci3,cd3),sumMatriz(ci4,cd4))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (c1(i)(j))
          else if (i < mitad && j >= mitad) (c2(i)(j-mitad))
          else if(i >= mitad && j < mitad) (c3(i-mitad)(j))
          else (c4(i-mitad)(j-mitad)))
      }      
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
      if (m1.length ==1) (Vector(Vector(m1(0)(0) * m2(0)(0))))
      else{
        val mitad = m1.length/2
        val (a11, a12, a21, a22) = (subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val (b11, b12, b21, b22) = (subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        
        val p1 = multStrassen(a11, restaMatriz(b12, b22))
        val p2 = multStrassen(sumMatriz(a11, a12), b22)
        val p3 = multStrassen(sumMatriz(a21, a22), b11)
        val p4 = multStrassen(a22, restaMatriz(b21, b11))
        val p5 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
        val p6 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))
        val p7 = multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12))


        /*val (s1, s2, s3, s4, s5) = (restaMatriz(b12, b22),sumMatriz(a11,a12),sumMatriz(a21,a22),restaMatriz(b21,b11),sumMatriz(a11,a22))
        val (s6, s7, s8, s9, s10) = (sumMatriz(b11,b22),restaMatriz(a12,a22),sumMatriz(b21,b22),restaMatriz(a11,a21),sumMatriz(b11,b12))
        val (p1, p2, p3, p4, p5, p6, p7) = (multStrassen(a11, s1),multStrassen(s2, b22),multStrassen(s3, b11),multStrassen(a22, s4),multStrassen(s5, s6),multStrassen(s7, s8),multStrassen(s9, s10))
        */
        val (c1, c2, c3, c4) = (sumMatriz(restaMatriz(sumMatriz(p5, p4),p2),p6),sumMatriz(p1, p2),sumMatriz(p3,p4),restaMatriz(restaMatriz(sumMatriz(p5, p1),p3),p7))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (c1(i)(j))
          else if (i < mitad && j >= mitad) (c2(i)(j-mitad))
          else if(i >= mitad && j < mitad) (c3(i-mitad)(j))
          else (c4(i-mitad)(j-mitad)))
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
    def multStrassenPar(m1:Matriz, m2:Matriz): Matriz ={
      if (m1.length <= math.pow(2,0)) (multStrassen(m1,m2))
      else{
        val mitad = m1.length/2
        val (a11, a12, a21, a22) = parallel(subMatriz(m1,0,0,mitad),subMatriz(m1,0,mitad,mitad),subMatriz(m1,mitad,0,mitad),subMatriz(m1,mitad,mitad,mitad))
        val (b11, b12, b21, b22) = parallel(subMatriz(m2,0,0,mitad),subMatriz(m2,0,mitad,mitad),subMatriz(m2,mitad,0,mitad),subMatriz(m2,mitad,mitad,mitad))
        
        /*val (s1, s2, s3, s4, s5) = (task(restaMatriz(b12, b22)),task(sumMatriz(a11,a12)),task(sumMatriz(a21,a22)),task(restaMatriz(b21,b11)),task(sumMatriz(a11,a22)))
        val (s6, s7, s8, s9, s10) = (task(sumMatriz(b11,b22)),task(restaMatriz(a12,a22)),task(sumMatriz(b21,b22)),task(restaMatriz(a11,a21)),task(sumMatriz(b11,b12)))
        val (p1, p2, p3, p4, p5, p6, p7) = (task(multStrassenPar(a11,s1.join())),task(multStrassenPar(s2.join(),b22)),task(multStrassenPar(s3.join(),b11)),task(multStrassenPar(a22,s4.join())),task(multStrassenPar(s5.join(), s6.join())),task(multStrassenPar(s7.join(), s8.join())),task(multStrassenPar(s9.join(), s10.join())))
        */

        val p1 = task(multStrassen(a11, restaMatriz(b12, b22)))
        val p2 = task(multStrassen(sumMatriz(a11, a12), b22))
        val p3 = task(multStrassen(sumMatriz(a21, a22), b11))
        val p4 = task(multStrassen(a22, restaMatriz(b21, b11)))
        val p5 = task(multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22)))
        val p6 = task(multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22)))
        val p7 = task(multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12)))

        
        val (c1, c2, c3, c4) = parallel(sumMatriz(restaMatriz(sumMatriz(p5.join(), p4.join()),p2.join()),p6.join()),sumMatriz(p1.join(), p2.join()),sumMatriz(p3.join(),p4.join()),restaMatriz(restaMatriz(sumMatriz(p5.join(), p1.join()),p3.join()),p7.join()))
        
        Vector.tabulate(m1.length,m1.length)((i,j) => 
          if (i < mitad && j < mitad) (c1(i)(j))
          else if (i < mitad && j >= mitad) (c2(i)(j-mitad))
          else if(i >= mitad && j < mitad) (c3(i-mitad)(j))
          else (c4(i-mitad)(j-mitad)))
      }
    }  

    //--------------------------
    def multStrassenPrueba(m1: Matriz, m2: Matriz): Matriz = {
    // Implementar el algoritmo de Strassen
    // Suponiendo que las matrices m1 y m2 son cuadradas, entonces
    // m1.length == m2.length

    // Caso base: si la matriz es de tamaño 1x1
    if (m1.length == 1 && m2.length == 1) Vector(Vector(m1(0)(0) * m2(0)(0)))
    else {
      // Caso recursivo
      val n = m1.length / 2
      val a11 = subMatriz(m1, 0, 0, n)
      val a12 = subMatriz(m1, 0, n, n)
      val a21 = subMatriz(m1, n, 0, n)
      val a22 = subMatriz(m1, n, n, n)

      val b11 = subMatriz(m2, 0, 0, n)
      val b12 = subMatriz(m2, 0, n, n)
      val b21 = subMatriz(m2, n, 0, n)
      val b22 = subMatriz(m2, n, n, n)

      val p1 = multStrassen(a11, restaMatriz(b12, b22))
      val p2 = multStrassen(sumMatriz(a11, a12), b22)
      val p3 = multStrassen(sumMatriz(a21, a22), b11)
      val p4 = multStrassen(a22, restaMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p6 = multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22))
      val p7 = multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12))

      val c11 = sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6)
      val c12 = sumMatriz(p1, p2)
      val c21 = sumMatriz(p3, p4)
      val c22 = restaMatriz(sumMatriz(p5, p1), sumMatriz(p3, p7))

      Vector.tabulate(m1.length, m1.length)((i, j) =>
        // Si i y j están en la primera mitad de la matriz
        if (i < n && j < n) c11(i)(j)
        // Si i está en la primera mitad y j en la segunda
        else if (i < n && j >= n) c12(i)(j - n)
        // Si i está en la segunda mitad y j en la primera
        else if (i >= n && j < n) c21(i - n)(j)
        // Si i y j están en la segunda mitad
        else c22(i - n)(j - n)
      )
    }
  }

  def multStrassenParPrueba(m1: Matriz, m2: Matriz): Matriz ={
    // Igual a multStrassen pero se debe paralelizar con parallel el cálculo
    // Suponiendo que las matrices m1 y m2 son cuadradas, entonces
    // m1.length == m2.length

    // Caso base (umbral): si la matriz es de tamaño 2^3x2^3
    if (m1.length <= math.pow(2, 3)) multStrassen(m1, m2)
    else {
      // Caso recursivo
      val n = m1.length / 2

      val (a11, a12, a21, a22) = parallel(
        subMatriz(m1, 0, 0, n),
        subMatriz(m1, 0, n, n),
        subMatriz(m1, n, 0, n),
        subMatriz(m1, n, n, n)
      )
      val (b11, b12, b21, b22) = parallel(
        subMatriz(m2, 0, 0, n),
        subMatriz(m2, 0, n, n),
        subMatriz(m2, n, 0, n),
        subMatriz(m2, n, n, n)
      )

      // Usar task para paralelizar el cálculo de los productos
      val p1 = task(multStrassen(a11, restaMatriz(b12, b22)))
      val p2 = task(multStrassen(sumMatriz(a11, a12), b22))
      val p3 = task(multStrassen(sumMatriz(a21, a22), b11))
      val p4 = task(multStrassen(a22, restaMatriz(b21, b11)))
      val p5 = task(multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22)))
      val p6 = task(multStrassen(restaMatriz(a12, a22), sumMatriz(b21, b22)))
      val p7 = task(multStrassen(restaMatriz(a11, a21), sumMatriz(b11, b12)))

      val (c11, c12, c21, c22) = parallel(
        sumMatriz(
          restaMatriz(sumMatriz(p5.join(), p4.join()), p2.join()),
          p6.join()
        ),
        sumMatriz(p1.join(), p2.join()),
        sumMatriz(p3.join(), p4.join()),
        restaMatriz(
          sumMatriz(p5.join(), p1.join()),
          sumMatriz(p3.join(), p7.join())
        )
      )

      Vector.tabulate(m1.length, m1.length)((i, j) =>
        // Si i y j están en la primera mitad de la matriz
        if (i < n && j < n) c11(i)(j)
        // Si i está en la primera mitad y j en la segunda
        else if (i < n && j >= n) c12(i)(j - n)
        // Si i está en la segunda mitad y j en la primera
        else if (i >= n && j < n) c21(i - n)(j)
        // Si i y j están en la segunda mitad
        else c22(i - n)(j - n)
      )
    }
  }

    //--------------------------------------    
}

object Main {
  def main(args: Array[String]): Unit = {
    import Matrices._
    import Benchmark._
    for{
      i <- 1 to 10
      m1 = matrizAlAzar(math.pow(2,i).toInt,2)
      m2 = matrizAlAzar(math.pow(2,i).toInt,2)
    } yield (println("Comparación Strassen, i= "+i+" "+compararAlgoritmos(multStrassen,multStrassenPar)(m1,m2)))
    
    //for (n <- 0 to 10000001 by 1000000) yield(println("Comparación Vectores, n= "+n+" "+compararProdPunto(n)))
    

    /*val m21 = matrizAlAzar(math.pow(2,10).toInt,2)
    val m22 = matrizAlAzar(math.pow(2,10).toInt,2)

    println(multMatriz(m21,m22))
    println(multMatrizPar(m21,m22))
    println(multMatrizRec(m21,m22))
    println(multMatrizRecPar(m21,m22))
    println(multStrassen(m21,m22))
    println(multStrassenPar(m21,m22))*/
    
  }    
}
