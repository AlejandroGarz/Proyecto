package taller
//viva scala
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RiegoOptimoTest extends AnyFunSuite {

  test("Costo de riego finca paralelo") {
    val finca = Vector((10, 3, 4), (5, 3, 3), (2, 2, 1), (8, 1, 1), (6, 4, 2))
    val distancia = Vector(
      Vector(0, 2, 2, 4, 4),
      Vector(2, 0, 4, 2, 6),
      Vector(2, 4, 0, 2, 2),
      Vector(4, 2, 2, 0, 4),
      Vector(4, 6, 2, 4, 0)
    )
    val pi = Vector(0, 1, 4, 2, 3)

    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val costoRiego = riegoOptimo.costoRiegoFincaPar(pi)

    assert(costoRiego == 8)
  }

  test("Costo de movilidad paralelo") {
    val finca = Vector((10, 3, 4), (5, 3, 3), (2, 2, 1), (8, 1, 1), (6, 4, 2))
    val distancia = Vector(
      Vector(0, 2, 2, 4, 4),
      Vector(2, 0, 4, 2, 6),
      Vector(2, 4, 0, 2, 2),
      Vector(4, 2, 2, 0, 4),
      Vector(4, 6, 2, 4, 0)
    )
    val pi = Vector(0, 1, 4, 2, 3)

    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val costoMovilidad = riegoOptimo.costoMovilidadPar(pi)

    assert(costoMovilidad == 12)
  }

  test("Costo total paralelo") {
    val finca = Vector((10, 3, 4), (5, 3, 3), (2, 2, 1), (8, 1, 1), (6, 4, 2))
    val distancia = Vector(
      Vector(0, 2, 2, 4, 4),
      Vector(2, 0, 4, 2, 6),
      Vector(2, 4, 0, 2, 2),
      Vector(4, 2, 2, 0, 4),
      Vector(4, 6, 2, 4, 0)
    )
    val pi = Vector(0, 1, 4, 2, 3)

    val riegoOptimo = new RiegoOptimo(finca, distancia)
    val costoTotal = riegoOptimo.costoTotalPar(pi)

    assert(costoTotal != 30)
  }
}
