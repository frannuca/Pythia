package fjn.pythia.analytics.optimizers.interiorPoint

import fjn.pythia.analytics.optimizers.nonlinear.generalizedFunction._
import scalala.tensor.dense.DenseMatrix
import fjn.pythia.analytics.optimizers.nonlinear.{nonLinearFunctionTrait, derivativesTrait}

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 1/4/12
 * Time: 9:05 PM
 * To change this template use File | Settings | File Templates.
 */

import util.Random

class lagrangeMultiplier (f: DenseMatrix[Double] => Double,x0:DenseMatrix[Double] ,constraints:List[DenseMatrix[Double] => Double])
extends derivativesTrait with nonLinearFunctionTrait{

  var ck:Double = 1000;
  
  private def pFunc2(xy:DenseMatrix[Double]):Double = {

     val x:DenseMatrix[Double] = DenseMatrix.zeros[Double](x0.numRows,1)
     val y:DenseMatrix[Double] = DenseMatrix.zeros[Double](xy.numRows - x0.numRows,1)



     var n = 0
     while(n < x0.numRows){
       x(n,0)= xy(n,0)
       n += 1
     }

     n = 0
     while(n < xy.numRows - x0.numRows){
       y(n,0)= xy(n + x0.numRows ,0)
       n += 1
     }

     var r = f(x)
     n = 0
     while(n < y.numRows){
       val cctr = constraints(n)(x)
       r += y(n,0) *  cctr
       r +=   0.5 * ck * cctr*cctr
       n += 1
     }

     r
   }


  val pFunc = pFunc2 _
  def lagrangeFunctional(x:DenseMatrix[Double]):Double={
    pFunc(x)   
  }
  
  def solve(nIter:Int):DenseMatrix[Double]={
 

    //iterate over the sequence of optimization problems:
    var k = 0;
    var xStart = x0;

    var xy = DenseMatrix.rand(xStart.numRows + constraints.length,1)  *0.00001

    var n = 0
    while(n<xStart.numRows)
    {
      xy(n,0)=xStart(n,0)
      n += 1
    }
    while(k<50)
    {
      val cg = new CGSecantPolakRibiere(lagrangeFunctional,tolerance=1e-5,sigma0=1e-5,jmax=3)


      //kth optimization problem:

          n = 0


          val xres = cg.iterate(200,xy)


           while(n<xStart.numRows)
                {
                  xStart(n,0) = xres(n,0)
                  n += 1
                }

                while(n<xy.numRows)
                {
                  val gx = constraints(n-xStart.numRows)(xStart)
                  xy(n,0) = xy(n,0) + ck *  gx
                  n += 1
                }


          ck = ck * 1.1;
      
          xy = xres.copy
      
      k += 1
    }

    xy
    
  }


}