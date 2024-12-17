package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class generarProgramacionesRiegoPar extends AnyFunSuite {

    test("Test para generar programacion de riego"){
        val finca1 = Vector((1, 2, 4), (3, 4, 1), (4, 2, 3))
        val distancia = Vector(
            Vector(0, 2, 2, 4, 4),
            Vector(2, 0, 4, 2, 6),
            Vector(2, 4, 0, 2, 2),
            Vector(4, 2, 2, 0, 4),
            Vector(4, 6, 2, 4, 0))

            val objRiego = new RiegoOptimo(finca1, distancia)
            val programacionRiego = objRiego.generarProgramacionesRiegoPar()
            val expected = Vector(Vector(0, 1, 2), Vector(0, 2, 1), Vector(1, 0, 2), Vector(1, 2, 0), Vector(2, 0, 1), Vector(2, 1, 0))
            assert(programacionRiego === expected)
    }
}