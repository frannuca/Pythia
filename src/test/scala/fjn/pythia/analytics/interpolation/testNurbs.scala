package fjn.pythia.analytics.interpolation

import org.specs2.mutable.Specification
import fjn.pythia.matrix.Matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/29/12
 * Time: 8:24 PM
 * To change this template use File | Settings | File Templates.
 */

class testNurbs  extends Specification {
  "Creating a optimization test for SWARM" should {

    "Converge to given solution" in {
      `testAlgorithm` mustEqual true
    }

  }

  
  def `testAlgorithm` ={
    val qk =
      (for(h <- 0 until 21;
        val mt = new Matrix[Double](2,1)
      ) yield {mt.zeros;val mt2=mt+h.toDouble; mt2}
            ).toArray[Matrix[Double]]
    


    val bspline = new Nurbs(qk,Array(2,2))
    true
  }

}
