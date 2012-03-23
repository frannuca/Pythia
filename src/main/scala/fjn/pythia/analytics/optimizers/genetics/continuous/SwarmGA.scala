package fjn.pythia.analytics.optimizers.genetics.continuous

import commons.{SwarmPop}
import fjn.pythia.analytics.optimizers.genetics.discrete.commons.tAlgorithm
import org.apache.commons.lang.NotImplementedException
import fjn.pythia.matrix.Matrix


/**
* User: fran
* Date: 1/18/12
* Time: 7:14 PM
*/

/**
 *
 * @param pFitness
 * @param numberOfCluster
 * @param numberOfParticlePerCluster
 * @param minLimit
 * @param maxLimit
 * @param velocityMomentum
 * @param towardsGlobalAcceleration
 * @param towardsClusterAcceleration
 * @param towardsBestParticleAcceleration
 */
case class SwarmGA(pFitness:(Array[Double]) => Double,numberOfCluster:Int,
                      numberOfParticlePerCluster:Int,
                      minLimit:Array[Double],maxLimit:Array[Double],
                      velocityMomentum:Double, 
                      towardsGlobalAcceleration:Double,
                      towardsClusterAcceleration:Double, 
                      towardsBestParticleAcceleration:Double
                    )
                     extends tAlgorithm[Double]{


  
  
  require(minLimit.length == maxLimit.length && minLimit.length>0)

  val particleDimension = minLimit.length

  var population:SwarmPop=init()



  private def init():SwarmPop={

     new SwarmPop(numberOfCluster,numberOfParticlePerCluster,minLimit,maxLimit,pFitnessTranformation,
       towardsGlobalAcceleration,towardsClusterAcceleration,towardsBestParticleAcceleration,velocityMomentum)

  }
  def pFitnessTranformation(x:Matrix[Double]):Double =
  {
    pFitness(x.getArray())
  };




     /**
     * Evolution of the tAlgorithm is performed through calls to next
     */
     def next():Boolean={
       population.algorithmComposition(true)
     }







}