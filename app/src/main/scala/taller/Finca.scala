import scala.util.Random

class Finca {

    // Un tablon es una tripleta con el tiempo de supervivencia ,
    // el tiempo de riego y la prioridad del tablon
    // Un tablón es una tripleta que contiene:

    // - Tiempo de supervivencia (Int)
    // - Tiempo de riego (Int)
    // - Prioridad del tablón (Int)
    type Tablon = (Int, Int, Int)

    /// Una finca es un vector de tablones
    type Finca = Vector[Tablon]

    // Si f : Finca , f(i) = (tsi, tri, pi)
    // La distancia entre dos tablones se representa por
    // una matriz

    // La distancia entre dos tablones se representa mediante una matriz:
    // - Es un vector de vectores de enteros (Int)
    // - d(i)(j) indica la distancia entre los tablones i y j
    type Distancia = Vector[Vector[Int]]

    // Una programaci´on de riego es un vector que asocia
    // cada tablon i con su turno de riego (0 es el primer turno ,
    // n-1 es el ´ultimo turno)

    // Una programación de riego es un vector que asocia cada tablón con su turno de riego
    // - 0 es el primer turno, n-1 es el último turno
    // - Cada programación es una permutación de {0, ..., n-1}
    type ProgRiego = Vector[Int]

    // Si v : ProgRiego , y v.length == n, v es una permutaci´on
    // de {0, ..., n-1} v(i) es el turno de riego del tablon i
    // para 0 <= i < n
    // El tiempo de inicio de riego es un vector que asocia
    // cada tablon i con el momento del tiempo en que se riega
    
    // El tiempo de inicio de riego es un vector:
    // - t(i) indica el momento en que inicia el riego del tablón i
    type TiempoInicioRiego = Vector[Int]

    // Si t : TiempoInicioRiego y t.length == n, t(i) es la hora a
    // la que inicia a regarse el tablon i

/*-------------------------------------*/

    val random = new Random ( )
    // Función para crear una finca con valores aleatorios
    def fincaAlAzar(long: Int): Finca = {
        // Crea una finca de long tablones ,
        // con valores aleatorios entre 1 y long * 2 para el tiempo
        // de supervivencia , entre 1 y long para el tiempo
        // de regado y entre 1 y 4 para la prioridad
        val v = Vector.fill(long)(
            (random.nextInt(long * 2) + 1,  // Tiempo de supervivencia 
            random.nextInt(long) + 1,       // Tiempo de regado
            random.nextInt(4) + 1)          // Prioridad       
        )
        v
    }

    // Función para crear una matriz de distancias aleatorias
    def distanciaAlAzar(long: Int): Distancia = {
        // Crea una matriz de distancias para una finca
        // de long tablones , con valores aleatorios entre
        // 1 y long * 3
        val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
        Vector.tabulate(long, long)((i, j) =>
            if (i < j) v(i)(j)       // Valor para la parte superior de la matriz
            else if (i == j) 0       // La diagonal principal es 0
            else v(j)(i))            // Simetría para la parte inferior de la matriz
    }

    // Función para obtener el tiempo de suministro del tablón i
    def tSup(f: Finca, i: Int): Int = {
        f(i)._1
    }

    // Función para obtener el tiempo de riego del tablón i
    def tReg(f: Finca, i: Int): Int = {
        f(i)._2
    }

    // Función para obtener la prioridad del tablón i
    def prio(f: Finca, i: Int): Int = {
        f(i)._3
    }

    def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
        // Dada una finca f y una programaci´on de riego pi,
        // y f.length == n, tIR(f, pi) devuelve t: TiempoInicioRiego
        // tal que t(i) es el tiempo en que inicia el riego del
        // tablon i de la finca f seg´un pi
        val tiempos = Array.fill(f.length)(0) // Vector de tiempos inicializado en 0
        
        // Asignar explícitamente el tiempo de inicio para el primer tablón
        val primerTablon = pi(0)
        tiempos(primerTablon) = 0
        
        // Calcular el tiempo de inicio para los demás tablones en la secuencia pi
        for (j <- 1 until pi.length) {
            val prevTablon = pi(j - 1) // Tablón anterior en la programación
            val currTablon = pi(j)     // Tablón actual
            tiempos(currTablon) = tiempos(prevTablon) + tReg(f, prevTablon)
        }
        
        tiempos.toVector // Convertir el Array a un Vector antes de devolver
    }

/*-----------------------------------------------------------------------------------*/

    // Calcula el costo de regar un tablón específico
    def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
        val tiempoInicio = tIR(f, pi)(i)                 // Tiempo de inicio del riego
        val tiempoFinal = tiempoInicio + tReg(f, i)     // Tiempo final del riego
        if (tSup(f, i) - tReg(f, i) >= tiempoInicio) {  // No se pasa del tiempo de supervivencia
            tSup(f, i) - tiempoFinal
        } else {                                        // Penalización por prioridad
            prio(f, i) * (tiempoFinal - tSup(f, i))
        }
    }

    // Calcula el costo total de riego para toda la finca
    def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    (0 until f.length).map(i => costoRiegoTablon(i, f, pi)).sum
    }

    // Calcula el costo de movilidad del sistema de riego entre los tablones
    def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    (0 until pi.length - 1).map(j => d(pi(j))(pi(j + 1))).sum
    }

    // Genera todas las posibles programaciones de riego para la finca
    def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
        // Creamos un vector con los índices de los tablones de la finca
        val indices = (0 until f.length).toVector
        // Generamos todas las permutaciones de los índices y las convertimos en un Vector
        indices.permutations.toVector
    }

    // Encuentra la programación de riego óptima para una finca dada y su matriz de distancias
    def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
        // Dada una finca devuelve la programaci´on
        // de riego ´optima
        // Generar todas las posibles programaciones de riego
        val programaciones = generarProgramacionesRiego(f)
        // Calcular el costo total (riego + movilidad) para cada programación
        val costos = programaciones.map(pi =>
            (pi, costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d))
        )
        // Encontrar la programación con el costo mínimo
        costos.minBy(_._2)
    }

}
