
package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix
import scala.Double

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/23/12
 * Time: 11:03 PM
 * To change this template use File | Settings | File Templates.
 */


/**The user must provide an array of samples points which the algorithm
 uses as control points.
 The presentatin of the control points is performed as a lineal array of points plus
 a list of dimensions. Typically if our points are part of a rectangular grid
 the we need to provide an array such as [q00,q01,q02,...,q0(n-1),q10,...q1(n-1),...,q(m-1)0,..,q(m-1)(n-1)]
 */
trait controlPoints {
  ///Matrix of points to be interpolated, only in matrix form we
  ///can perform the 2-dimensional interpolation.
  ///the algorithm expects a list of number of samples per dimension
  ///The array of sample in dimension dk is extracted as follows:
  // item(i,j)=qk(j*dim_i+i)
  val qk: Array[Matrix[Double]] //list of control points as a multi-dimensional grid arrangement
  protected val tqk_Centripetal:Array[Matrix[Double]] //list of transformed control point in the target NURBS space
  protected val tqk_Chord:Array[Matrix[Double]] //list of transformed control point in the target NURBS space
  protected val tqk_EquallySpaced:Array[Matrix[Double]]
  val dim:Seq[Int] /// list of dimension composing our grid
  val viewer = new MultiArrayView[Matrix[Double]](qk,dim)
  
  def apply(w:Seq[Int]):Matrix[Double]=
  {
     viewer(w)
  }
}


/**
 * This trait computes the location of the control points into the target nurbs space
 */
trait parameterVector {
  self: controlPoints =>

  //Extracting the positions of each axis:
  private val parameterKnotsAux = 
    (for ( i <- 0 until self.dim.length ) yield Seq[Double]()).toArray


  //Find the list of points per coordinate:  
  for ( (sz,nCoord) <- self.dim  zip (0 until self.dim.length) )
  {
    for (i <- 0 until sz) {
        
          val m = (for (j <- 0 until self.dim.length) yield 0).toArray[Int]
          m(nCoord)=i
          
          parameterKnotsAux(nCoord) = parameterKnotsAux(nCoord) ++ Seq(viewer(m)(nCoord,0))
        
    
      }  
  }
  

  //this other Array hosts the list of location per axis but ordered
  private val orderedParameterKnots = parameterKnotsAux.map(s => s.sortWith((a, b) => a < b))

  //Calculating the final parameter knots associated to the sequence of points qk
  // with the Chord method
  val parametersKnots_Chord = (for ( i <- 0 until parameterKnotsAux.length ) yield Seq[Double]()).toArray
  val parametersKnots_EquallySpaced = (for ( i <- 0 until parameterKnotsAux.length ) yield Seq[Double]()).toArray
  val parametersKnots_Centripetal = (for ( i <- 0 until parameterKnotsAux.length ) yield Seq[Double]()).toArray

  for (i <- 0 until orderedParameterKnots.length) {


    //Calculate the sum of the differences between samples per coordinate for the Chord distribution
    //normalization
    val normLength =
      (for (n <- 1 until orderedParameterKnots(i).length;
            val d = orderedParameterKnots(i)(n) - orderedParameterKnots(i)(n - 1)
      ) yield d).toList.foldLeft(0.0)((acc, v) => acc + v)


    //Calculate the sum of the sqrt differences between samples per coordinate for the Centripetal
    //distribution normalization
    val sqrt_normLength =
      (for (n <- 1 until orderedParameterKnots(i).length;
            val d = math.sqrt(orderedParameterKnots(i)(n) - orderedParameterKnots(i)(n - 1))
      ) yield d).toList.foldLeft(0.0)((acc, v) => acc + v)


    parametersKnots_Chord(i)=parametersKnots_Chord(i) ++ Seq(0.0)


    parametersKnots_EquallySpaced(i)=parametersKnots_EquallySpaced(i) ++ Seq(0.0)


    parametersKnots_Centripetal(i)=parametersKnots_Centripetal(i) ++ Seq(0.0)



    var acc_chord = 0.0d
    var acc_eq = 0.0d
    var acc_centrip = 0.0d

    for (n <- 1 until orderedParameterKnots(i).length - 1) {
      parametersKnots_Chord(i)=
        parametersKnots_Chord(i) ++  Seq(parametersKnots_Chord(i)(n-1) +( orderedParameterKnots(i)(n) - orderedParameterKnots(i)(n - 1)) / normLength)
      parametersKnots_EquallySpaced(i) = parametersKnots_EquallySpaced(i) ++ Seq(n.toDouble / (orderedParameterKnots(i).length - 1).toDouble)
      parametersKnots_Centripetal(i) = parametersKnots_Centripetal(i) ++ Seq(parametersKnots_Centripetal(i)(n-1)+math.sqrt(math.abs(parameterKnotsAux(i)(n) - parameterKnotsAux(i)(n - 1)) ) / sqrt_normLength)
    }

    parametersKnots_Chord(i)  =
      parametersKnots_Chord(i) ++ Seq(1.0d)

    parametersKnots_EquallySpaced(i)  =
      parametersKnots_EquallySpaced(i) ++ Seq(1.0d)

    parametersKnots_Centripetal(i)  =
          parametersKnots_Centripetal(i) ++ Seq(1.0d)
  }


  val tqk_Centripetal =
  (for (i <- 0 until qk.length) yield {
        val q:Matrix[Double] = qk(i).clone()
        val xy = viewer.fromIndex2Seq(i)
        for ((n,v) <- (0 until xy.length) zip xy)
        {
          q.set(n,0,parametersKnots_Centripetal(n)(v))
        }
       q
      }).toArray[Matrix[Double]]

  val tqk_EquallySpaced =
    (for (i <- 0 until qk.length) yield {
          val q:Matrix[Double] = qk(i).clone()
          val xy = viewer.fromIndex2Seq(i)
          for ((n,v) <- (0 until xy.length) zip xy)
          {
            q.set(n,0,parametersKnots_EquallySpaced(n)(v))
          }
         q
        }).toArray[Matrix[Double]]

  val tqk_Chord =
    (for (i <- 0 until qk.length) yield {
          val q:Matrix[Double] = qk(i).clone()
          val xy = viewer.fromIndex2Seq(i)
          for ((n,v) <- (0 until xy.length) zip xy)
          {
            q.set(n,0,parametersKnots_Chord(n)(v))
          }
         q
        }).toArray[Matrix[Double]]

  //  def getParameterVector():Array[Matrix[Double]]=
  //  {
  //    orderedParameterKnots.foreach()
  //  }
}

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

  val knots_Centripetal = computeKnots(self.parametersKnots_Centripetal)
  val knots_Chords = computeKnots(self.parametersKnots_Chord)
  val knots__EquallySpaced= computeKnots(self.parametersKnots_EquallySpaced)

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

  def NCentripetal(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots_Centripetal)(i,p,nCoord)(u)
  def NChords(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots_Chords)(i,p,nCoord)(u)
  def NEquallySpaced(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots__EquallySpaced)(i,p,nCoord)(u)

}




