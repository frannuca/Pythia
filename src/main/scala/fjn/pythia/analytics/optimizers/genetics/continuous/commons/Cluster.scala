package fjn.pythia.analytics.optimizers.genetics.continuous.commons

import scalala.tensor.dense.DenseMatrix
import fjn.pythia.matrix.Matrix
import java.util.Random

/**
 * User: fran
 * Date: 1/23/12
 * Time: 2:06 PM
 */

case class Component(minLimit: Double, maxLimit: Double)

case class Particle(min: Array[Double], max: Array[Double]) {

  require(min.length == max.length)
  require(min.length > 0)

  val size = min.length
  var pNow: Matrix[Double] = new Matrix[Double](size, 1)
  var pBest: Matrix[Double] = new Matrix[Double](size, 1)
  var velocity = new Matrix[Double](size, 1)
  var fitnessValueNow: Double = 1e9
  var bestFitnessValueNow: Double = 1e9


  private val rndGen = new Random()




  val limits: Array[Component] =
    (for (n <- 0 until size;
          val b = Component(min(n), max(n))
    ) yield b).toArray


  for (i <- 0 until size) {
    pNow.set(i, 0, min(i) + rndGen.nextDouble() * (-min(i) + max(i)))
    pBest.set(i, 0, pNow(i, 0))
  }


  for (i <- 0 until velocity.numberRows) {
    velocity.set(i, 0, 0.1*rndGen.nextDouble() * (max(i) - min(i)))
  }


  def updateBestTarget() = {
    pNow.getArray().copyToArray(pBest.getArray())
  }


  override def clone(): Particle = {
    val newParticle = Particle(this.min, this.max)
    newParticle.bestFitnessValueNow = this.bestFitnessValueNow
    newParticle.fitnessValueNow = this.fitnessValueNow
    newParticle.velocity = this.velocity.clone();
    newParticle.pNow = this.pNow.clone()
    newParticle.pBest = this.pBest.clone()

    newParticle
  }

  
  override def toString():String = {
    val strBld = new StringBuilder()
    strBld.append("pnow=" )append pNow.toString() append  "\n" append
           "pBest=" append pBest.toString

    strBld.toString()
  }
}

case class Cluster(numberOfParticles: Int, minLimit: Array[Double], maxLimit: Array[Double]) {


  val particles: Array[Particle] =
    (for (i <- 0 until numberOfParticles;
          val part = Particle(minLimit, maxLimit)
    ) yield part).toArray

  var clusterBestParticle: Particle = Particle(minLimit, maxLimit)


  def apply(n: Int): Particle = {
    particles(n);
  }

  def size(): Int = particles.length


}

class SwarmPop(numberOfCluster: Int, numberOfParticlesPerCluster: Int, minLimit: Array[Double], maxLimit: Array[Double], pFunc: (Matrix[Double]) => Double,
               c1: Double, c2: Double, c3: Double, w: Double) {

  private val randGen = new Random()


  val listOfClusters =
    (for (i <- 0 until numberOfCluster;
          val c = Cluster(numberOfParticlesPerCluster, minLimit, maxLimit)
    ) yield c).toArray

  computerFitness(true)

  var gBest = Particle(minLimit, maxLimit)


  private def updateBestParticle(continue: Boolean):Boolean = {

    if (continue) {
      try {
        listOfClusters.foreach(
          cluster => {
            cluster.particles.foreach(
              particle => {
                if (particle.bestFitnessValueNow < cluster.clusterBestParticle.bestFitnessValueNow) {
                  cluster.clusterBestParticle = particle.clone()
                }
              }
            )
            if (cluster.clusterBestParticle.bestFitnessValueNow < this.gBest.bestFitnessValueNow) {
              gBest = cluster.clusterBestParticle.clone()
            }
          }
        )

        true
      }
      catch {
        case _ => false;
      }
    }
    else false

  }

  private def computerFitness(continue:Boolean):Boolean = {
    if (continue)
    {
      listOfClusters.foreach(
            cluster => {
              cluster.particles.foreach(
                particle => {

                  particle.fitnessValueNow = pFunc(particle.pNow)
                  if (particle.fitnessValueNow < particle.bestFitnessValueNow) {
                    particle.bestFitnessValueNow = particle.fitnessValueNow
                    particle.pBest = particle.pNow.clone()
                  }
                }
              )
            }
          )
      true
    }
    else
      false

  }

  private def moveParticles(continue:Boolean): Boolean = {


    if(continue){
      var allParticlesInBoundaries = true
          for (cluster <- listOfClusters) {

            for (particle <- cluster.particles) {

              val r1 = randGen.nextDouble()
              val r2 = randGen.nextDouble()
              val r3 = randGen.nextDouble()

              val  newv =
                particle.velocity * w +
                  (particle.pBest - particle.pNow) * r1 * c1 +
                  (gBest.pNow - particle.pNow) * r2 * c2 +
                  (cluster.clusterBestParticle.pBest - particle.pNow) * r3 * c3


              particle.velocity = particle.velocity + newv;
              particle.pNow = particle.pNow + particle.velocity
              ////
              //Todo: the particle must stay in boundaries, implement here bouncing algorithm
              ////
              var counter = 0

              allParticlesInBoundaries = true
              while (allParticlesInBoundaries && counter < 50) {
                counter = counter + 1
                val offendingInboundary = {
                  val cArray = particle.pNow.getArray()
                  for (i <- 0 until cArray.length;
                       val r =
                       if (particle.min(i) > cArray(i)) {
                         Some((i, -1, particle.min(i)))
                       }
                       else if (cArray(i) > particle.max(i)) {
                         Some((i, 1, particle.max(i)))
                       }
                       else None

                  ) yield r
                }

                allParticlesInBoundaries =
                  offendingInboundary.forall(z => z match {
                    case None => true;
                    case Some(k) => false
                  })

                offendingInboundary.filter(z => z match {
                  case None => false;
                  case Some(k) => true
                })

                offendingInboundary.flatMap(a => a).foreach(
                  p => {
                    val i = p._1
                    val isMax = p._2 > 0

                    var x = particle.pNow(i, 0)
                    val limit = p._3

                    if (isMax) {
                      x = limit - 0.3 * (x - limit)
                      particle.pNow.set(i, 0, x)
                    }
                    else
                    {
                      x = limit + 0.3 * (limit - x)
                      particle.pNow.set(i, 0, x)
                    }
                  }
                )
              }


            }
          }
          true

    }
    else false

  }


  val algorithmComposition=   ( (x:Boolean) => moveParticles(x)) andThen  (y=>computerFitness(y))  andThen ( (z:Boolean)=>updateBestParticle(z))

}

