package fjn.pythia.analytics.optimizers.nonlinear.solvers

import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.iterateTrait

/**
* Created by IntelliJ IDEA.
* User: fran
* Date: 12/26/11
* Time: 7:49 PM
* To change this template use File | Settings | File Templates.
*/

class SteepestDescentSolver(A:DenseMatrix[Double],b:DenseMatrix[Double],tolerance:Double = 1e-6)
 extends iterateTrait {

  def iterate(n:Int,x0:DenseMatrix[Double]):DenseMatrix[Double]={
    var i = 0
    var r:DenseMatrix[Double] =  (b - A*x0).toDense
    var x = x0.copy

    var delta:DenseMatrix[Double] =  (r.t * r).toDense
    val delta0 =  delta.copy
    val niter = n
    while(i<niter && delta(0,0) > tolerance*delta0(0,0)){
      val q = (A * r).toDense
      val denom = (r.t * q)
      val alpha:Double = delta(0,0)/ denom(0,0)

      x = x +  (r * alpha).toDense

      if (i % 50 == 0)
        r = b - A*x
      else
        r = r -  q * alpha

      delta = r.t * r
      i += 1

    }

    x
  }

}