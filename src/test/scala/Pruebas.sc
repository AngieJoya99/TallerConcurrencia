/* Angie Joya - 2322609
Emily Nuñez - 2240156*/

import Matrices._

val m01 = matrizAlAzar(math.pow(2,0).toInt,2)
val m02 = matrizAlAzar(math.pow(2,0).toInt,2)
val m11 = matrizAlAzar(math.pow(2,1).toInt,2)
val m12 = matrizAlAzar(math.pow(2,1).toInt,2)
val m21 = matrizAlAzar(math.pow(2,2).toInt,2)
val m22 = matrizAlAzar(math.pow(2,2).toInt,2)
val m71 = matrizAlAzar(math.pow(2,7).toInt,2)
val m72 = matrizAlAzar(math.pow(2,7).toInt,2)
val m101 = matrizAlAzar(math.pow(2,10).toInt,2)
val m102 = matrizAlAzar(math.pow(2,10).toInt,2)

//Matrices de tamaño 2^0
multMatriz(m01,m02)
multMatrizPar(m01,m02)
multMatrizRec(m01,m02)
multMatrizRecPar(m01,m02)
multStrassen(m01,m02)
multStrassenPar(m01,m02)

//Matrices de tamaño 2^1
multMatriz(m11,m12)
multMatrizPar(m11,m12)
multMatrizRec(m11,m12)
multMatrizRecPar(m11,m12)
multStrassen(m11,m12)
multStrassenPar(m11,m12)

//Matrices de tamaño 2^2
multMatriz(m21,m22)
multMatrizPar(m21,m22)
multMatrizRec(m21,m22)
multMatrizRecPar(m21,m22)
multStrassen(m21,m22)
multStrassenPar(m21,m22)

//Matrices de tamaño 2^7
multMatriz(m71,m72)
multMatrizPar(m71,m72)
multMatrizRec(m71,m72)
multMatrizRecPar(m71,m72)
multStrassen(m71,m72)
multStrassenPar(m71,m72)

//Matrices de tamaño 2^10
multMatriz(m101,m102)
multMatrizPar(m101,m102)
multMatrizRec(m101,m102)
multMatrizRecPar(m101,m102)
multStrassen(m101,m102)
multStrassenPar(m101,m102)