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

  var pNow: Matrix[Double] = new Matrix[Double](size, 1)
  var pBest: Matrix[Double] = new Matrix[Double](size, 1)
  var velocity = new Matrix[Double](size, 1)
  var fitnessValueNow: Double = 0.0
  var bestFitnessValueNow: Double = 0.0


  private val rndGen = new Random()


  val size = min.length

  val limits: Array[Component] =
    (for (n <- 0 until size;
          val b = Component(min(n), max(n))
    ) yield b).toArray


  for (i <- 0 until size) {
    pNow.set(i, 0, min(i) + rndGen.nextDouble() * (-min(i) + max(i)))
    pBest.set(i, 0, pNow(i, 0))
  }


  for (i <- 0 until velocity.numberRows) {
    velocity.set(i, 0, rndGen.nextDouble() * (max(i) - min(i)))
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
               c1: Double, c2: Double, w: Double) {

  private val randGen = new Random()


  val listOfClusters =
    (for (i <- 0 until numberOfCluster;
          val c = Cluster(numberOfParticlesPerCluster, minLimit, maxLimit)
    ) yield c).toArray

  var gBest = Particle(minLimit, maxLimit)



  private def updateBestParticle(): Unit = {

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

  }

  private def computerFitness(): Unit = {
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

    updateBestParticle()
  }

  private def moveParticles(): Boolean = {


    var allParticlesInBoundaries = true
    for (cluster <- listOfClusters) {

      for (particle <- cluster.particles) {

        val r1 = randGen.nextDouble()
        val r2 = randGen.nextDouble()

        particle.velocity =
          particle.velocity * w +
            (particle.pBest - particle.pNow) * r1 * c1 +
            (gBest.pNow - particle.pNow) * r2 * c2


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
              case None => false;
              case Some(k) => true
            })

          offendingInboundary.filter(z => z match {
            case None => false;
            case Some(k) => true
          })

          offendingInboundary.flatMap(a => a).foreach(
            p => {
              val i = p._1
              val isMax = p._2 > 0

              if (isMax) {
                var x = particle.pNow(i, 0)
                val limit = p._3

                x = limit - 0.65 * (x - limit)
                particle.pNow.set(i, 0, x)
              }
            }
          )
        }


      }
    }
    allParticlesInBoundaries

  }
}

