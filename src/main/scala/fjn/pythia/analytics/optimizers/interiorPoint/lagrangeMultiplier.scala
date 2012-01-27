package fjn.pythia.analytics.optimizers.interiorPoint

import fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction._
import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.{nonLinearFunctionTrait, derivativesTrait}

/**
 * User: fran
 * Date: 1/4/12
 * Time: 9:05 PM
 */

import util.Random

class lagrangeMultiplier (f: DenseMatrix[Double] => Double,x0:DenseMatrix[Double],
                          ck0:DenseMatrix[Double],
                          lk0:DenseMatrix[Double],
                          constraints:List[DenseMatrix[Double] => Double])
{

  private def pFunc2(x:DenseMatrix[Double])(ck:DenseMatrix[Double],lk:DenseMatrix[Double]):Double = {


     var r = f(x)
     var n = 0

     while(n < lk.numRows){
       val cctr = constraints(n)(x)
       val v = lk(n,0)
       val w = ck(n,0)
       r = r - v *  cctr
       r = r +   0.5 * w * cctr*cctr
       n += 1
     }

     r
   }

  def solve(nIter:Int):DenseMatrix[Double]={
 

    //iterate over the sequence of optimization problems:
    var k = 0;
    var xStart = x0.copy;

    var xy = DenseMatrix.rand(xStart.numRows + constraints.length,1)  *0.00001

    var lk = lk0.copy
    var ck = ck0.copy

    var n = 0

    while(k<nIter)
    {
      val cg = new CGSecantPolakRibiere(pFunc2(_)(ck,lk),tolerance=1e-5,sigma0=1e-5,jmax=5)


      //kth optimization problem:

          n = 0


          val xres = cg.iterate(200,xStart)




          while(n<lk.numRows)
          {
            val gx = constraints(n)(xStart)
            lk(n,0) = lk(n,0) - ck(n,0) *  gx
            n += 1
          }


          ck = ck * 1.01;
      
          xStart = xres.copy
      
      k += 1
    }

    xStart
    
  }


}