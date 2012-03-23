package fjn.pythia.analytics.optimizers.genetics.continuous

import org.specs2.mutable.Specification

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/21/12
 * Time: 8:04 AM
 * To change this template use File | Settings | File Templates.
 */

class testSWARM extends Specification {
  "Creating a optimization test for SWARM" should {

    "Converge to given solution" in {
      `testAlgorithm` mustEqual true
    }

  }


  def `testAlgorithm` = {

    val swarm = new SwarmGA(localfitness, 5, 10, List(-10d, -10d).toArray, List(10d, 10d).toArray, 0.00, 0.5, 0.25, 0.25)

    for (i <- 0 until 100) {
      swarm.next();
    }

    println(swarm.population.gBest.toString())
    println(swarm.population.gBest.bestFitnessValueNow)
    swarm.population.gBest.bestFitnessValueNow<1e-3
  }

  def localfitness(x: Array[Double]): Double = {


    val offsets = (0 until x.length).map(g => g % x.length)
    var i = 0;
    var aux: Double = 0.0
    for (t <- x) {
      aux = aux + (t - offsets(i)) * (t - offsets(i))
      i = i + 1
    }


    aux;


  }
}
