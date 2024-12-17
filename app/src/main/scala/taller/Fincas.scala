
package taller

import scala.util.Random

class Fincas {

    // Definición de tipos
    type Tablon = (Int, Int, Int) // (Tiempo de supervivencia, Tiempo de riego, Prioridad)
    type Finca = Vector[Tablon]
    type Distancia = Vector[Vector[Int]]
    type ProgRiego = Vector[Int]
    type TiempoInicioRiego = Vector[Int]

    val random = new Random()

    // Crear una finca con valores aleatorios
    def fincaAlAzar(long: Int): Finca = {
        Vector.fill(long)((
          random.nextInt(long * 2) + 1, // Tiempo de supervivencia
          random.nextInt(long) + 1,    // Tiempo de riego
          random.nextInt(4) + 1        // Prioridad
        ))
    }

    // Crear una matriz de distancias aleatorias
    def distanciaAlAzar(long: Int): Distancia = {
        val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
        Vector.tabulate(long, long)((i, j) =>
            if (i < j) v(i)(j) else if (i == j) 0 else v(j)(i)
        )
    }

    // Métodos auxiliares para obtener valores de un tablón
    def tSup(f: Finca, i: Int): Int = f(i)._1
    def tReg(f: Finca, i: Int): Int = f(i)._2
    def prio(f: Finca, i: Int): Int = f(i)._3

    // Calcular el tiempo de inicio de riego
    def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
        val tiempos = Array.fill(f.length)(0)
        tiempos(pi(0)) = 0
        for (j <- 1 until pi.length) {
            val prevTablon = pi(j - 1)
            val currTablon = pi(j)
            tiempos(currTablon) = tiempos(prevTablon) + tReg(f, prevTablon)
        }
        tiempos.toVector
    }

    // Calcular el costo de riego de un tablón
    def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
        val tiempoInicio = tIR(f, pi)(i)
        val tiempoFinal = tiempoInicio + tReg(f, i)
        if (tSup(f, i) - tReg(f, i) >= tiempoInicio) {
            tSup(f, i) - tiempoFinal
        } else {
            prio(f, i) * (tiempoFinal - tSup(f, i))
        }
    }

    // Calcular el costo total de riego para la finca
    def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
        (0 until f.length).map(i => costoRiegoTablon(i, f, pi)).sum
    }

    // Calcular el costo de movilidad entre tablones
    def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
        (0 until pi.length - 1).map(j => d(pi(j))(pi(j + 1))).sum
    }

    // Generar todas las programaciones de riego posibles
    def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
        (0 until f.length).toVector.permutations.toVector
    }

    // Encontrar la programación óptima de riego
    def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
        val programaciones = generarProgramacionesRiego(f)
        val costos = programaciones.map(pi =>
            (pi, costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d))
        )
        costos.minBy(_._2)
    }
}

