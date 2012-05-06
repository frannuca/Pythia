

package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix



/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/23/12
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */

class Nurbs(val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int]) extends controlPoints
    with parameterVector
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with solver1D
    {


  def  apply(t:Double):Matrix[Double] ={

      var sum = new Matrix[Double](2,1)
      for (i <- 0 until samples)
      {
        val pAux = new Matrix[Double](2,1);
        pAux.set(0,0,pk(i,0))
        pAux.set(1,0,pk(i,1))
        sum = sum +    pAux* NEquallySpaced (i,basisOrder(0),0)(t)
      }

    sum
  }

}


