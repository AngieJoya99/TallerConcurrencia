/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

import Matrices._
//Paralelización de tareas. De datos es solo para el producto punto. 
//Matrices binarias, inmutables

val m1 = matrizAlAzar(math.pow(2,2).toInt,2)
val m2 = matrizAlAzar(math.pow(2,2).toInt,2)

//Método Estándar
multMatriz(m1,m2)
multMatrizPar(m1,m2)

//Método Recursivo
multMatrizRec(m1,m2)
multMatrizRecPar(m1,m2)

//Método de Strassed
multStrassen(m1,m2)
multStrassenPar(m1,m2)

val m1 = Vector(Vector(0, 1, 1, 1), Vector(1, 1, 0, 0), Vector(1, 1, 0, 0), Vector(1, 1, 0, 0))
val m2 = Vector(Vector(0, 1, 1, 1), Vector(1, 1, 1, 0), Vector(1, 0, 1, 0), Vector(1, 0, 1, 0))
