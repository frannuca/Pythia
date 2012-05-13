
package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix
import scala.Double

/**
 * Created by fjn
 * User: fran
 * Date: 3/23/12
 * Time: 11:03 PM
 * To change this template use File | Settings | File Templates.
 */





trait BasisFunctionOrder
{
  val basisOrder:Array[Int]
}

trait KnotsVector {
  self:parameterVector with BasisFunctionOrder=>






  def computeKnots(params:Array[Seq[Double]]):Array[Seq[Double]]={
    val knots_ = (for ( i <- 0 until params.length ) yield Seq[Double]()).toArray
    for (i <- 0 until params.length)
    {
      val p = basisOrder(i)
      val dim = params(i).length
      for (j <- 0 until p+1)
      {
        knots_(i) = knots_(i) ++ Seq(0.0)
      }
      for (jaux <- 1 to dim-p-1)
      {
        val j = jaux+p
        knots_(i) = knots_(i) ++ Seq(
                  (for (k <- j-p to j;
                   val v:Double = params(i)(k)/(p+1)
                  ) yield v).foldLeft(0.0)((acc,vv)=> acc+vv)
                  )

      }

      for (j <- 0 until p+1)
      {
        knots_(i) = knots_(i) ++ Seq(1.0)
      }
    }
    knots_
  }

  val knotsVector = computeKnots(self.parameterKnots)
}

trait Basis {
  self:KnotsVector with   BasisFunctionOrder =>

   private def N(knots:Array[Seq[Double]])(i: Int, p: Int,nCoord:Int)(u: Double): Double = {
    if (p == 0) {

      if (knots(nCoord)(i) <= u && u < knots(nCoord)(i + 1))
        1.0
      else if( knots(nCoord)(i + 1) == 1 && u==1)
        1.0
      else
        0.0
    }
    else {
      val denom1 = (knots(nCoord)(i + p) - knots(nCoord)(i));
      val denom2 =  (knots(nCoord)(i + p + 1) - knots(nCoord)(i + 1))
      val num1 =  (u - knots(nCoord)(i))
      val num2 =   (knots(nCoord)(i + p + 1) - u)
      val comp1 =
        if(math.abs(denom1)>1e-6){
        num1 / denom1 * N(knots)(i, p - 1,nCoord)(u)
      }
      else 0
      val comp2 = if(math.abs(denom2)>1e-6) {
        num2 / denom2 * N(knots)(i + 1, p - 1,nCoord)(u)
      }
      else 0

      comp1+comp2
    }
  }

  def NBasis(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knotsVector)(i,p,nCoord)(u)
}




