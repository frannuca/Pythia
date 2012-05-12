package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/8/12
 * Time: 7:41 AM
 * To change this template use File | Settings | File Templates.
 */


trait solver1D {
  self: Basis with parameterVector with controlPoints with BasisFunctionOrder=>
  val samples:Int = self.qk.length
  var pk: Matrix[Double] = new Matrix[Double](1,1)
  val weights:Array[Double] = new Array[Double](self.qk.length)

  def solve(z:Array[Double]):Boolean={

      val listOfMatrix =
        for (k <- 0 until dim.length)
        yield
        {
          val qMatrix = new Matrix[Double](samples,samples)
          for (i <- 0 until samples)
          {
            for (j <- 0 until  samples)
            {
              val kaux = k
              val iaux = i
              val jaux = j
              val paux =   basisOrder(k)
              val vv = NEquallySpaced(j,basisOrder(k),k)(tqk_EquallySpaced(i)(k,0))
              qMatrix.set(i,j,vv )

            }

          }
          qMatrix

        }

      var rightM = new Matrix[Double](samples,dim.length+1)
      for(i <- 0 until samples)
      {
        for(j <- 0 until dim.length)
          rightM.set(i,j,tqk_EquallySpaced(i)(j,0))

        rightM.set(i,dim.length,z(i))
      }

      var mSol = new Matrix[Double](samples,dim.length+1)
      //computing the contol points:
      for (m <- listOfMatrix)
      {
        m.invert()
        rightM = m * rightM
      }

      pk = rightM


    true
    }


}

