

package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix



/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/23/12
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */


class Nurbs1D(val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int])
    extends controlPoints
    with parameterVector
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with solver1D{
  def  apply(t:Double):Matrix[Double] ={

  //      var sum = new Matrix[Double](2,1)
  //      for (i <- 0 until samples)
  //      {
  //        val pAux = new Matrix[Double](2,1);
  //        pAux.set(0,0,pk(i,0))
  //        pAux.set(1,0,pk(i,1))
  //        sum = sum +    pAux* NEquallySpaced (i,basisOrder(0),0)(t)
  //      }
  //
  //    sum
      new Matrix[Double](1,1)
}
}


class Nurbs2D(val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int])
  extends controlPoints
      with parameterVector
      with BasisFunctionOrder
      with KnotsVector
      with Basis
      with solver2D{

  def  apply(u:Double,v:Double):Matrix[Double] ={

        var sum = new Matrix[Double](3,1)
        for (i <- 0 until dim(0))
        {
          for (j <- 0 until dim(1))
          {
            val pAux = new Matrix[Double](3,1);
            pAux.set(0,0,pk(i)(j,0))
            pAux.set(1,0,pk(i)(j,1))
            pAux.set(2,0,pk(i)(j,2))
            val basis = (NEquallySpaced(i,basisOrder(0),0)(u)*NEquallySpaced(j,basisOrder(1),1)(v))
            sum = sum +    pAux * basis
          }

        }

      sum
  }
      }

