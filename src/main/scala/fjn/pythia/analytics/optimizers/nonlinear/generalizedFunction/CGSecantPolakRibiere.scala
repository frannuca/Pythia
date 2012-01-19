package fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction

import fjn.pythia.analytics.commons.{NNMatrixExtensions}

import scalala.tensor.dense.DenseMatrix
import sun.reflect.generics.reflectiveObjects.NotImplementedException
import fjn.pythia.analytics.optimizers.nonlinear.{derivativesTrait, nonLinearFunctionTrait, CGTrait}


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 12/31/11
 * Time: 4:33 PM
 * To change this template use File | Settings | File Templates.
 */

class CGSecantPolakRibiere(val pFunc:DenseMatrix[Double] => Double,
                           val tolerance:Double=1e-3,
                           sigma0:Double = 0.01,
                           val jmax:Int=10)
  extends CGTrait with nonLinearFunctionTrait with derivativesTrait {

  def iterate(numberIterations:Int,x0:DenseMatrix[Double]):DenseMatrix[Double]={


    var x = x0.copy
    val jmax= 10;


    var i:Int = 0
    var k:Int = 0

    var r = _grad(x0) * -1.0
    var M = _hessian(x0)
    var invM = M.copy
    applyFunction( v => if (v!=0) 1.0/v else 0.0,invM)

    var s:DenseMatrix[Double] = invM * r

    var d:DenseMatrix[Double] = s.copy

    var deltaNew:Double = (r.t * d).toDense(0,0)
    var delta0 =  deltaNew

    while ( i < numberIterations && deltaNew > tolerance * delta0){
      var j = 0
      var deltaD:Double = (d.t * d).toDense(0,0)
      var alpha:Double = -sigma0
      var xeval:DenseMatrix[Double] = x + d * sigma0
      var thPrev:Double = (_grad(xeval).t * d).toDense(0,0)

      do{
        val th:Double =  (_grad(x).t * d).toDense(0,0)

        alpha =   alpha * th/(thPrev - th)
        if (alpha.isNaN || alpha.isInfinity)
          alpha = 0.0

        x = x + d * alpha

        thPrev = th
        j += 1
      } while(j < jmax && alpha*alpha*deltaD>tolerance)

      r  = _grad(x) * -1.0
      val deltaOld = deltaNew
      val deltaMid = (r.t * s).toDense(0,0)

      M = _hessian(x)
      invM = M.copy
      applyFunction( v => if (v!=0) 1.0/v else 0.0,invM)
      s = invM * r
      deltaNew = (r.t * s).toDense(0,0)

      val beta:Double = (deltaNew - deltaMid)/ deltaOld

      k += 1
      if(k == x.numRows || beta <= 1e-10)
      {
        d = s.copy
        k = 0
      }
      else
      {
        d = s +  d * beta
      }

      println(i.toString+"->"+pFunc(x))
      i += 1
    }
    x

    }
}