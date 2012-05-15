package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/9/12
 * Time: 8:08 AM
 * To change this template use File | Settings | File Templates.
 */

trait Solver2D  {
  self: Basis with parameterVector with controlPoints with BasisFunctionOrder=>
  val samples:Int = self.qk.length

  val weights:Array[Double] = new Array[Double](self.qk.length)
  var pk:Seq[Matrix[Double]] = Seq()
  
  def SolveOnU(z:Array[Double],viewer_norm:MultiArrayView[Matrix[Double]],
               viewer_original:MultiArrayView[Matrix[Double]],viewerZ:MultiArrayView[Double]):Seq[Matrix[Double]]=
  {
         //Preparing the matrix for constant y-slices:
        val qXMatrix = new Matrix[Double](dim(0),dim(0))
        for (i <- 0 until dim(0)) {
          val uk = viewer_norm(Seq(i, 0))(0, 0)
          for (j <- 0 until dim(0)) {

            val vv = NBasis(j, basisOrder(0), 0)(uk)
            qXMatrix.set(i, j, vv)
          }
        }



        qXMatrix.invert()



        //Solving linear systems for y index (x,0),(x,1),....,(x,m-1) with x 0 .. n-1
        //var Rl: Seq[Matrix[Double]] =
           for (l <- 0 until dim(1))//we have dim(1) dim(1) points in y-direction, which are slices now
            yield{

             //Building linear system for the kth slice:

             //Basis function matrix:

             val sampleSize=3 //(x,y,z)
             //sample vector(right side of the system):
             var rightM = new Matrix[Double](dim(0),sampleSize)
                   for(i <- 0 until dim(0))
                   {
                     val auxPos = Seq(i,l)
                     val posq = viewer_original(auxPos)
                     for(j <- 0 until sampleSize-1)
                     {

                       rightM.set(i,j,posq(j,0))
                     }


                     val auxZ = viewerZ(auxPos)
                     rightM.set(i,sampleSize-1,auxZ)
                   }

             val auxVal = qXMatrix * rightM
             auxVal
            }


  }
  def solve(z:Array[Double]):Boolean={


    val viewer_normalized = new MultiArrayView[Matrix[Double]](tqk,dim)
    val viewer_original = new MultiArrayView[Matrix[Double]](qk,dim)
    val viewerZ = new MultiArrayView[Double](z,dim)

    val Rl = SolveOnU(z,viewer_normalized,viewer_original,viewerZ);

    val qXMatrix = new Matrix[Double](dim(1),dim(1))
        for (i <- 0 until dim(1)) {
          val vk = viewer_normalized(Seq(0, i))(1, 0)
          for (j <- 0 until dim(1)) {
            val vv = NBasis(j, basisOrder(1), 1)(vk)
            qXMatrix.set(i, j, vv)
          }
        }

        qXMatrix.invert()



         pk=
           for (k <- 0 until dim(0))//we have dim(1) dim(1) points in y-direction, which are slices now
            yield{

             val sampleSize=3 //(x,y,z)
             //sample vector(right side of the system):
             var rightM = new Matrix[Double](dim(1),sampleSize)
                   for(i <- 0 until dim(1))
                   {
                     for(j <- 0 until sampleSize)
                       rightM.set(i,j,Rl(i)(k,j))
                   }

             qXMatrix * rightM
            }


    true
    }

}