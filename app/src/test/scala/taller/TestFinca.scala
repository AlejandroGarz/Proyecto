package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

import scala.util.Random


class FincaTest extends AnyFunSuite {
    val finca = new Finca()
    
    test("fincaAlAzar genera una finca válida") {
        val f = finca.fincaAlAzar(3)
        assert(f.length == 3)
        assert(f.forall { case (tsup, treg, prio) =>
        tsup > 0 && treg > 0 && prio > 0 && prio <= 4
        })
    }

    test("distanciaAlAzar genera una matriz de distancias simétrica") {
        val d = finca.distanciaAlAzar(3)
        assert(d.length == 3)
        assert(d.forall(row => row.length == 3))
        for (i <- 0 until d.length; j <- 0 until d.length) {
        assert(d(i)(j) == d(j)(i)) // Verificar simetría
        if (i == j) assert(d(i)(j) == 0) // Diagonal principal es 0
        }
    }

    test("tSup, tReg y prio devuelven valores correctos") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        assert(finca.tSup(f, 0) == 10)
        assert(finca.tReg(f, 1) == 7)
        assert(finca.prio(f, 2) == 1)
    }

    test("tIR calcula correctamente los tiempos de inicio de riego") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        val pi = Vector(2, 0, 1) // Orden: Tablón 2 -> Tablón 0 -> Tablón 1
        val tiempos = finca.tIR(f, pi)
        assert(tiempos == Vector(6, 11, 0)) // 6 para tablón 2, 11 para tablón 0, 0 para tablón 1
    }

    test("costoRiegoTablon calcula costos correctamente") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        val pi = Vector(2, 0, 1)
        val costo = finca.costoRiegoTablon(2, f, pi)
        assert(costo == 4) // Ejemplo calculado manualmente
    }

    test("costoRiegoFinca suma costos correctamente") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        val pi = Vector(2, 0, 1)
        val costoTotal = finca.costoRiegoFinca(f, pi)
        assert(costoTotal == 20) // Ejemplo calculado manualmente
    }

    test("costoMovilidad calcula correctamente el costo de movilidad") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        val pi = Vector(2, 0, 1)
        val d = Vector(
        Vector(0, 4, 6),
        Vector(4, 0, 2),
        Vector(6, 2, 0)
        )
        val costoMov = finca.costoMovilidad(f, pi, d)
        assert(costoMov == 8) // 6 (de 2 a 0) + 2 (de 0 a 1)
    }

    test("generarProgramacionesRiego genera todas las permutaciones") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        val programaciones = finca.generarProgramacionesRiego(f)
        assert(programaciones.length == 6) // 3! = 6 permutaciones
        assert(programaciones.contains(Vector(0, 1, 2)))
        assert(programaciones.contains(Vector(2, 1, 0)))
    }

    test("ProgramacionRiegoOptimo encuentra la programación óptima") {
        val f = Vector((10, 5, 3), (15, 7, 2), (20, 6, 1))
        val d = Vector(
        Vector(0, 4, 6),
        Vector(4, 0, 2),
        Vector(6, 2, 0)
        )
        val (optima, costo) = finca.ProgramacionRiegoOptimo(f, d)
        assert(optima == Vector(2, 0, 1)) // Ejemplo calculado manualmente
        assert(costo == 28) // Suma de costos de riego y movilidad
    }
}
