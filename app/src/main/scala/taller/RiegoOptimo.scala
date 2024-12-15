import scala.collection.parallel.CollectionConverters._
package taller{

class RiegoOptimo(finca: Vector[(Int, Int, Int)], distancia: Vector[Vector[Int]]) {

  private def costoRiegoTablon(index: Int, finca: Vector[(Int, Int, Int)], pi: Vector[Int]): Int = {
    val (ts, tr, p) = finca(index)
    val tInicio = pi.indexOf(index) * tr
    if (tInicio > ts) p * (tInicio - ts) else 0
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
}}
