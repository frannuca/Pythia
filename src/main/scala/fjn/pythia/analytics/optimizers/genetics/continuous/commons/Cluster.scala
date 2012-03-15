package fjn.pythia.analytics.optimizers.genetics.continuous.commons

import scalala.tensor.dense.DenseMatrix
import fjn.pythia.matrix.Matrix

/**
 * User: fran
 * Date: 1/23/12
 * Time: 2:06 PM
 */



case class Component[T](minLimit:T, maxLimit:T)(implicit m2: Manifest[T], implicit val m: Fractional[T])
  
case class Particle[T](min:Array[T], max:Array[T])(implicit m2: Manifest[T], implicit val m: Fractional[T])
{    
  
  require(min.length == max.length)
  require(min.length>0)
  val size = min.length
  
  val limits:Array[Component[T]]=
    (for (n <- 0 until size;
           val b = Component[T](min(n),max(n))
      ) yield b).toArray
  
  
  private var pNow:Matrix[T]= new Matrix[T](size,1)
  private var pBest:Matrix[T]= new Matrix[T](size,1)
  
  def apply() = pNow
  def getBest = pBest

}

case class Cluster[T](numberOfParticles:Int,minLimit:Array[T],maxLimit:Array[T])(implicit m2: Manifest[T], implicit val m: Fractional[T])
{

    val particles:Array[Particle[T]]=
     ( for (i <- 0 until numberOfParticles;
                   val part = Particle(minLimit,maxLimit)
              )yield part).toArray

    val clusterBestParticle:Particle[T] =  Particle(minLimit,maxLimit)
  
  
  def apply(n:Int):Particle[T]=
  {
    particles(n);
  }

  def size():Int=particles.length


}

case class SwarmPop[T](numberOfCluster:Int, numberOfParticlesPerCluster:Int, dim:Int,minLimit:Array[T], maxLimit:Array[T])(implicit m2: Manifest[T], implicit val m: Fractional[T])
{
      
  val listOfClusters =
    (for ( i <- 0 until numberOfCluster;
         val c = Cluster[T](numberOfParticlesPerCluster,minLimit,maxLimit)
    ) yield c).toArray

    val populationBestParticle = Particle[T](minLimit,maxLimit)
}