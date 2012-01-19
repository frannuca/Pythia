package fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction

import fjn.pythia.analytics.commons.{NNMatrixExtensions}
import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.{derivativesTrait, nonLinearFunctionTrait, CGTrait}

class CGNewtonRaphsonFletcherReeves(val pFunc:DenseMatrix[Double] => Double,
                                    val tolerance:Double=1e-1,
                                    val jmax:Int=10)
  extends CGTrait with nonLinearFunctionTrait with derivativesTrait{

  def iterate(numberIterations:Int,x0:DenseMatrix[Double]):DenseMatrix[Double]={


    
    var x = x0.copy

    var minVal = (x.copy,pFunc(x0))
    
    var i:Int = 0
    var k:Int = 0

    var r = _grad(x0) * -1.0

    var d:DenseMatrix[Double] = r.copy

    var deltaNew:Double = (r.t * r).toDense(0,0)
    var delta0 =  deltaNew

    while ( i < numberIterations && deltaNew > tolerance * delta0){
      var j = 0
      var deltaD:Double = (d.t * d).toDense(0,0)
      var alpha:Double = 0.0

      val Hf = _hessian(x)      
      
      
      
      

      val denom = (d.t * Hf * d).toDense(0,0)
      
      do{
          alpha =  -1.0 * ((_grad(x).t * d).toDense(0,0)/denom)
          if (alpha.isNaN || alpha.isInfinity)
            alpha = 0.0

          x = x + d * alpha

          j += 1

        
      } while(j < jmax && alpha*alpha*deltaD>tolerance)

      val valueNow = pFunc(x)
//      if(minVal._2>valueNow)
//      {
//        minVal = (x.copy,valueNow)
//      }
//      else
//      {
//        x = minVal._1.copy
//
//      }
      r  = _grad(x) * -1.0
      val deltaOld = deltaNew

      deltaNew = (r.t * r).toDense(0,0)

      val beta:Double = (deltaNew)/ deltaOld

      d = r +  d * beta

      k += 1
      if(k == x.numRows || beta <= 1e-10)
      {
        d = r.copy
        k = 0
      }

      println(i.toString+"->"+pFunc(x))
      

      i += 1
    }
    x

    }
}