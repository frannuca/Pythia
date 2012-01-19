package fjn.pythia.analytics.optimizers.nonlinear.solvers

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/15/11
 * Time: 8:04 AM
 * To change this template use File | Settings | File Templates.
 */

import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.iterateTrait

class ConjugateGradientSolver(val A:DenseMatrix[Double],val b:DenseMatrix[Double],val tolerance:Double)
  extends iterateTrait{

  def iterate(numberIterations:Int,x0:DenseMatrix[Double]):DenseMatrix[Double]={

    var i:Int = 0
    var r:DenseMatrix[Double] =  b -  A*x0
    var deltaNew:Double  = (r.t * r).toDense(0,0)
    var delta0:Double = deltaNew
    var d:DenseMatrix[Double] = r.copy

    var x = x0.copy

    while ( i < numberIterations && deltaNew > tolerance*delta0)  {
      val q:DenseMatrix[Double] =  A * d
      val alpha = deltaNew/(d.t*q).toDense(0,0)

      x = x.copy +  d * alpha
      if (i % 50 == 0){
        r = b - A * x
      }
      else{
        r = r.copy - q * alpha
      }

      val deltaOld = deltaNew
      deltaNew = (r.t * r).toDense(0,0)
      var beta = deltaNew/deltaOld

      d = r + d.copy * beta

      i += 1
    }

    x

  }


}