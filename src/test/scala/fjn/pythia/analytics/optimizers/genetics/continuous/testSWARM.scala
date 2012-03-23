package fjn.pythia.analytics.optimizers.genetics.continuous

import org.specs2.mutable.Specification

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/21/12
 * Time: 8:04 AM
 * To change this template use File | Settings | File Templates.
 */

class testSWARM extends Specification{
  "Creating a optimization test for SWARM" should {
  
      "Converge to given solution" in {
      `testAlgorithm` mustEqual true
      }
  
    }
  
  
      def `testAlgorithm` = {

           val swarm = new SwarmGA(localfitness,5,10,List(-10d,-10d).toArray,List(10d,10d).toArray,0.0,2,1.0,2.0)

        for(i<- 0 until 50)
          {
            swarm.next();
          }

        println(swarm.population.gBest.toString())
        println(swarm.population.gBest.bestFitnessValueNow)
           true
      }

  def localfitness(x:Array[Double]):Double ={

            val ve =
              for (t <- x;
            val aux:Double = (t-5)*(t-5)
            )yield aux


           ve.foldLeft(0.0)((a,v)=> a+v)
          }
}
