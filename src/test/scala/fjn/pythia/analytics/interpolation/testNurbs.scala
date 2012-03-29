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
    val qk = new Array[Matrix[Double]](3)
    
    qk(0)=new Matrix[Double](2,1)
    qk(0).zeros
   
    qk(1)=new Matrix[Double](2,1)
    qk(1).zeros
    qk(1)=qk(1) + 3


    qk(2)=new Matrix[Double](2,1)
    qk(2).zeros
    qk(2)=qk(1) + 5


    
    val bspline = new Nurbs(qk,Array(2,2))
  }

}
