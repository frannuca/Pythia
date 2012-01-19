package fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 12/30/11
 * Time: 6:59 PM
 * To change this template use File | Settings | File Templates.
 */

import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.{derivativesTrait, nonLinearFunctionTrait}

class SteepestDescent(val pFunc:DenseMatrix[Double]=>Double,
                      lr:Double = 1e-2,
                      beta:Double=0.1) extends nonLinearFunctionTrait with derivativesTrait{


  def iterate(numberOfIteration:Int,x0:DenseMatrix[Double]):DenseMatrix[Double]={



    var lr2 = lr
    var x = x0.copy
    var xBest = x0.copy


    var momentum = DenseMatrix.zeros[Double](x.numRows,x.numCols)

    var counter:Int = 0

    var minVal = pFunc(xBest)
    while(counter < numberOfIteration)
    {

      momentum += _grad(x) * lr
      x = x  - momentum
      momentum = momentum * beta


      val vNow = pFunc(x)
      if (minVal > vNow )
      {

        xBest = x.copy
        minVal =  vNow
        lr2 = 1.1

      }
      else
      {
        x = xBest.copy
        lr2 *= 0.1
        if(lr2<1e-9) lr2= 1e-9
      }

      counter += 1
    }


    x
  }
}