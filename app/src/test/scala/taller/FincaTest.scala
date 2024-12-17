package taller

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
@RunWith(classOf[JUnitRunner  ])
class FincasTest extends AnyFunSuite {

  val fincas = new Fincas()

  test("Crear finca al azar") {
    val finca = fincas.fincaAlAzar(5)
    assert(finca.length == 5)
    assert(finca.forall(t => t._1 > 0 && t._2 > 0 && t._3 > 0))
  }


  test("Crear matriz de distancias al azar") {
    val distancia = fincas.distanciaAlAzar(5)
    assert(distancia.length == 5)
    assert(distancia.forall(row => row.length == 5))
    assert(distancia.zipWithIndex.forall { case (row, i) => row(i) == 0 })
  }


  test("Tiempo de inicio de riego") {
    val finca = Vector((10, 2, 1), (8, 3, 2), (12, 1, 1))
    val pi = Vector(0, 2, 1)
    val tiempos = fincas.tIR(finca, pi)
    assert(tiempos == Vector(0, 3, 2))
  }


  test("Costo de riego de un tablón") {
    val finca = Vector((10, 2, 1), (8, 3, 2), (12, 1, 1))
    val pi = Vector(0, 2, 1)
    val costo = fincas.costoRiegoTablon(1, finca, pi)
    assert(costo == 2)
  }

  test("Costo total de riego de la finca") {
    val finca = Vector((10, 2, 1), (8, 3, 2), (12, 1, 1))
    val pi = Vector(0, 2, 1)
    val costo = fincas.costoRiegoFinca(finca, pi)
    assert(costo == 19)
  }

  test("Costo de movilidad entre tablones") {
    val finca = Vector((10, 2, 1), (8, 3, 2), (12, 1, 1))
    val pi = Vector(0, 2, 1)
    val distancias = Vector(
      Vector(0, 5, 3),
      Vector(5, 0, 2),
      Vector(3, 2, 0)
    )
    val costo = fincas.costoMovilidad(finca, pi, distancias)
    assert(costo == 5)
  }

  test("Programación óptima de riego") {
    val finca = Vector((10, 2, 1), (8, 3, 2), (12, 1, 1))
    val distancias = Vector(
      Vector(0, 5, 3),
      Vector(5, 0, 2),
      Vector(3, 2, 0)
    )
    val (optima, costo) = fincas.ProgramacionRiegoOptimo(finca, distancias)
    assert(costo == 22)
  }
}
