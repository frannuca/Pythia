//package fjn.pythia.analytics.optimizers.genetics.continuous
//
//import commons.SwarmPop
//import fjn.pythia.analytics.optimizers.genetics.discrete.commons.tAlgorithm
//import org.apache.commons.lang.NotImplementedException
//
//
///**
// * User: fran
// * Date: 1/18/12
// * Time: 7:14 PM
// */
//
///**
// *
// * @param pFitnessv: error function to be optimized
// * @param K: Number of clusters
// * @param M : Number of particle per cluster
// * @param dim : particle dimension (vector size)
// */
//class SwarmGA(val pFitnessv:(Array[Double]) => Double,K:Int, M:Int, dim:Int)  extends tAlgorithm{
//
//
//  lazy val numberOfCluster = K
//  lazy val numberOfParticlePerCluster = M
//  lazy val particleDimension = dim
//
//  private var population:SwarmPop
//
//  private def init={
//
//    population = new SwarmPop(numberOfCluster,numberOfParticlePerCluster,particleDimension)
//
//  }
//  val pFitness = pFitnessv;
//
//     /**
//     * Evolution of the tAlgorithm is performed through calls to next
//     */
//     def next():Boolean={
//      throw new NotImplementedException()
//     }
//
//
//
//
//
//
//
//}