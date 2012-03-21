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

           val swarm = new SwarmGA(localfitness,1,2,List(0d,0d).toArray,List(1d,1d).toArray,0.6,2,2,2)
           true
      }

  def localfitness(x:Array[Double]):Double ={

            val ve =
              for (t <- x;
            val aux:Double = java.lang.Math.sin((t-0.5)*0.5)/(java.lang.Math.abs((t-0.5)*0.5))
            )yield aux


            ve.foldLeft(0.0)((a,v)=> a+v)
          }
}
