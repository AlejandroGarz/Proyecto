package taller
import org.scalameter._
import scala.util.Random

object App {
  def main(args: Array[String]): Unit = {
    //se sube la medicion de los costos tanto secuencial como paralelo.
    type Tablon = (Int, Int, Int) 
    type Finca = Vector[Tablon]
    type Distancia = Vector[Vector[Int]]
    type ProgRiego = Vector[Int]

    def medicionSec() = {
      println("\n\nmedicion secuencial")
      val objFinca = new Fincas
      val finca: Finca = objFinca.fincaAlAzar(5) 
      val distancia: Distancia = objFinca.distanciaAlAzar(5)

      println(s"Finca generada: $finca")
      println(s"Matriz de distancias generada: $distancia")

      
      val programacionAleatoria: ProgRiego = Random.shuffle((0 until finca.length).toVector)
      println(s"Programación de riego generada: $programacionAleatoria")

      
      val tiempoCostoRiego = measure {
        val costoRiego = objFinca.costoRiegoFinca(finca, programacionAleatoria)
        println(s"Costo total de riego: $costoRiego")
      }

      println(s"Tiempo de ejecución del cálculo del costo de riego: $tiempoCostoRiego")

      
      val tiempoCostoMovilidad = measure {
        val costoMovilidad = objFinca.costoMovilidad(finca, programacionAleatoria, distancia)
        println(s"Costo total de movilidad: $costoMovilidad")
      }

      println(s"Tiempo de ejecución del cálculo del costo de movilidad: $tiempoCostoMovilidad")

    
      val tiempoOptimo = measure {
        val (progOptima, costoOptimo) = objFinca.ProgramacionRiegoOptimo(finca, distancia)
        println(s"Programación óptima: $progOptima, Costo óptimo: $costoOptimo")
      }

      println(s"Tiempo de ejecución para encontrar la programación óptima: $tiempoOptimo")

    }

    def medicionParalela() = {
      //Measure para la parte paralela  
      println("medicion paralela")  
      val random = new Random()
      val tamañoFinca = 10
      val finca: Finca = Vector.fill(tamañoFinca)((
            random.nextInt(tamañoFinca * 2) + 1, // Tiempo de supervivencia
            random.nextInt(tamañoFinca) + 1,    // Tiempo de riego
            random.nextInt(4) + 1))     // Prioridad

      val distancia: Distancia = Vector.tabulate(tamañoFinca, tamañoFinca)((i, j) => if (i == j) 0 else random.nextInt(10) + 1)

      println(s"Finca generada: $finca")
      println(s"Matriz de distancias generada: $distancia")

      
      val riegoOptimoPar = new RiegoOptimo(finca, distancia)
      val pi = (0 until finca.length).toVector 

      println(s"Programación de riego generada: $pi")

  
      val tiempoCostoRiegoPar = measure {
        val costoRiego = riegoOptimoPar.costoRiegoFincaPar(pi)
        println(s"Costo total de riego (paralelo): $costoRiego")
      }
      println(s"Tiempo para calcular costo de riego (paralelo): $tiempoCostoRiegoPar")

      val tiempoCostoMovilidadPar = measure {
        val costoMovilidad = riegoOptimoPar.costoMovilidadPar(pi)
        println(s"Costo total de movilidad (paralelo): $costoMovilidad")
      }
      println(s"Tiempo para calcular costo de movilidad (paralelo): $tiempoCostoMovilidadPar")

      val tiempoGeneracionPar = measure {
        val programaciones = riegoOptimoPar.generarProgramacionesRiegoPar()
        println(s"Total de programaciones generadas (paralelo): ${programaciones.length}")
      }
      println(s"Tiempo para generar programaciones de riego (paralelo): $tiempoGeneracionPar")

      val tiempoCostoTotalPar = measure {
        val costoTotal = riegoOptimoPar.costoTotalPar(pi)
        println(s"Costo total (paralelo): $costoTotal")
      }
      println(s"Tiempo para calcular costo total (paralelo): $tiempoCostoTotalPar")

    
      val tiempoOptimoPar = measure {
        val (progOptima, costoOptimo) = riegoOptimoPar.ProgramacionRiegoOptimoPar()
        println(s"Programación óptima paralela: $progOptima, Costo óptimo: $costoOptimo")
      }
      println(s"Tiempo para encontrar la programación óptima (paralelo): $tiempoOptimoPar")
    }

    val medicionP = medicionParalela()
    val medicionSecuencial = medicionSec()
    println(s"Tiempo de ejecución paralela: $medicionP")
    println(s"Tiempo de ejecución secuencial: $medicionSecuencial")
    
  }
}
