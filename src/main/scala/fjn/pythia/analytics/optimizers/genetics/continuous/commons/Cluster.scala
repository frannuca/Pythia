package fjn.pythia.analytics.optimizers.genetics.continuous.commons

import scalala.tensor.dense.DenseMatrix

/**
 * User: fran
 * Date: 1/23/12
 * Time: 2:06 PM
 */


class Particle(dim:Int)
{
  var pBest=DenseMatrix.zeros[Double](dim,1)
  var pNow=DenseMatrix.zeros[Double](dim,1)  
}

class Cluster(K:Int,dim:Int)
{
  val listOfParticles =
  (for ( i <- 0 until K;
       val p = new Particle(dim)
  ) yield p).toArray

  val clusterBestParticle = DenseMatrix.zeros[Double](dim,1)

}

class SwarmPop(numberOfCluster:Int, numberOfParticlesPerCluster:Int, dim:Int)
{
  val listOfClusters =
    (for ( i <- 0 until numberOfCluster;
         val p = new Cluster(numberOfParticlesPerCluster,dim)
    ) yield p).toArray

    val populationBestParticle = DenseMatrix.zeros[Double](dim,1)
  
}