package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ProgramacionRiegoOptimoPar extends AnyFunSuite {

  test("Programación de riego óptima para un caso simple") {
    val finca = Vector((10, 3, 4), (5, 3, 3), (2, 2, 1))
    val distancia = Vector(
      Vector(0, 2, 4),
      Vector(2, 0, 6),
      Vector(4, 6, 0)
    )

    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val (mejorProgramacion, costoTotal) = riegoOptimo.ProgramacionRiegoOptimoPar()
    val mejorProgramacionEsperada = Vector(1, 0, 2)
    val costoTotalEsperado = 8

    assert(mejorProgramacion == mejorProgramacionEsperada)
    assert(costoTotal == costoTotalEsperado)
  }

  test("Programación de riego óptima para una finca más grande") {
    val finca = Vector((10, 3, 4), (5, 3, 3), (2, 2, 1), (8, 1, 1), (6, 4, 2))
    val distancia = Vector(
      Vector(0, 2, 2, 4, 4),
      Vector(2, 0, 4, 2, 6),
      Vector(2, 4, 0, 2, 2),
      Vector(4, 2, 2, 0, 4),
      Vector(4, 6, 2, 4, 0)
    )

    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val (mejorProgramacion, costoTotal) = riegoOptimo.ProgramacionRiegoOptimoPar()
    val mejorProgramacionEsperada = Vector(4, 2, 1, 0, 3)
    val costoTotalEsperado = 15

    assert(mejorProgramacion == mejorProgramacionEsperada)
    assert(costoTotal == costoTotalEsperado)
  }

  test("Programación de riego con distancias iguales") {
    val finca = Vector((10, 3, 4), (5, 3, 3), (2, 2, 1), (8, 1, 1))
    val distancia = Vector(
      Vector(0, 1, 1, 1),
      Vector(1, 0, 1, 1),
      Vector(1, 1, 0, 1),
      Vector(1, 1, 1, 0)
    )

    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val (mejorProgramacion, costoTotal) = riegoOptimo.ProgramacionRiegoOptimoPar()

    val mejorProgramacionEsperada = Vector(1, 2, 0, 3)
    val costoTotalEsperado = 3

    assert(mejorProgramacion == mejorProgramacionEsperada)
    assert(costoTotal == costoTotalEsperado)
  }

  test("Programación de riego con finca trivial (1 tablón)") {
    val finca = Vector((10, 3, 4))
    val distancia = Vector(
      Vector(0)
    )
    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val (mejorProgramacion, costoTotal) = riegoOptimo.ProgramacionRiegoOptimoPar()

    val mejorProgramacionEsperada = Vector(0)
    val costoTotalEsperado = 0

    assert(mejorProgramacion == mejorProgramacionEsperada)
    assert(costoTotal == costoTotalEsperado)
  }
}
