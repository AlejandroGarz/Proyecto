package taller
import scala.collection.parallel.CollectionConverters._
class RiegoOptimo(finca: Vector[(Int, Int, Int)], distancia: Vector[Vector[Int]]) {
  type ProgRiego = Vector [ Int ]                                                                                                                                          //Hay problemas? Franco resuelve

  private def costoRiegoTablon(index: Int, finca: Vector[(Int, Int, Int)], pi: Vector[Int]): Int = {
    val (ts, tr, p) = finca(index)
    val tInicio = pi.indexOf(index) * tr
    val tFinal = tInicio + tr
    if (tInicio > ts) {
      p * (tInicio - ts)
    } else if (tFinal > ts) {
      p * (tFinal - ts)
    } else {
      0
    }
  }

  def costoRiegoFincaPar(pi: Vector[Int]): Int = {
    (0 until finca.length).par.map(i => costoRiegoTablon(i, finca, pi)).sum
  }

  def costoMovilidadPar(pi: Vector[Int]): Int = {
    (0 until pi.length - 1).par.map(j => distancia(pi(j))(pi(j + 1))).sum
  }

  def costoTotalPar(pi: Vector[Int]): Int = {
    costoRiegoFincaPar(pi) + costoMovilidadPar(pi)
  }


  def generarProgramacionesRiegoPar(): Vector[ProgRiego] = {
      val indices = (0 until finca.length).toVector
      indices.permutations.toVector.par.toVector
  }

  def ProgramacionRiegoOptimoPar(): (ProgRiego, Int) = {
    val programaciones = generarProgramacionesRiegoPar()
    val costos = programaciones.par.map(pi =>
      (pi, costoRiegoFincaPar(pi) + costoMovilidadPar(pi))
    )
    costos.minBy(_._2)
  }
}
