package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix
import java.util.ArrayList
import org.codehaus.jackson.map.ser.BasicSerializerFactory

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/23/12
 * Time: 11:03 PM
 * To change this template use File | Settings | File Templates.
 */


trait controlPoints {
  val qk: Array[Matrix[Double]]
  val tqk:Array[Matrix[Double]]
  val dim = qk(0).numberRows
}

trait parameterVector {
  self: controlPoints =>

  //We define the parameter knot location as an array of array of points
  //marking the position of each axis:
  private val parameterKnotsAux = 
    (for ( i <- 0 until self.dim ) yield Seq[Double]()).toArray


  //Find the list of points per coordinate:
  for (i <- 0 until self.qk.length) {
    for (n <- 0 until self.qk(i).numberRows) {
      parameterKnotsAux(n) = parameterKnotsAux(n) ++ Seq(self.qk(i)(n, 0))
    }

  }

  //this other Array hosts the list of location per axis but ordered
  private val orderedParameterKnots = parameterKnotsAux.map(s => s.sortWith((a, b) => a < b))

  //Calculating the final parameter knots associated to the sequence of points qk
  // with the Chord method
  val parametersKnots_Chord = (for ( i <- 0 until self.dim ) yield Seq[Double]()).toArray
  val parametersKnots_EquallySpaced = (for ( i <- 0 until self.dim ) yield Seq[Double]()).toArray
  val parametersKnots_Centripetal = (for ( i <- 0 until self.dim ) yield Seq[Double]()).toArray

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

  val tqk =
  (for (i <- 0 until qk.length) yield {        
        val q = qk(i).clone()
        val ar = q.getArray().map(x => {parametersKnots_EquallySpaced(i)(orderedParameterKnots.indexOf(x))})


          for (j <- 0 until ar.length)
          { q.set(j,0,ar(j))};

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
      for (j <- 0 until dim)
      {
        if( j  <=  p)
        {
          knots_(i) = knots_(i) ++ Seq(0.0)
        }
        else if( j  > (dim - p - 1) )
        {
          knots_(i) = knots_(i) ++ Seq(1.0)
        }
        else
        {
          knots_(i) = knots_(i) ++ Seq(
          (for (k <- (j-p) until j+1;
           val v:Double = params(i)(k)/(p+1.0)
          ) yield v).foldLeft(0.0)((acc,vv)=> acc+vv)
          )

        }


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
      else
        0.0
    }
    else {
      (u - knots(nCoord)(i)) / (knots(nCoord)(i + p) - knots(nCoord)(i)) * N(knots)(i, p - 1,nCoord)(u) +
        (knots(nCoord)(i + p + 1) - u) / (knots(nCoord)(i + p + 1) - knots(nCoord)(i + 1)) * N(knots)(i + 1, p - 1,nCoord)(u)
    }
  }

  def NCentripetal(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots_Centripetal)(i,p,nCoord)(u)
  def NChords(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots_Chords)(i,p,nCoord)(u)
  def NEquallySpaced(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots__EquallySpaced)(i,p,nCoord)(u)

}

trait solver {
  self: Basis with parameterVector with controlPoints with BasisFunctionOrder=>

  val samples = self.qk.length
  val pk = new  Array[Matrix[Double]](self.qk.length)
  val weights = new Array[Double](self.qk.length)
  def solve()={


    
    val listOfMatrix =
      for (k <- 0 until dim)
      yield
    {
      val qMatrix = new Matrix[Double](samples,samples)
      for (i <- 0 until samples)
      {
        for (j <- 0 until  samples)
        {
          qMatrix.set(i,j, NEquallySpaced(i,basisOrder(i),k)(parametersKnots_EquallySpaced(i)(k)))
          //(i: Int, p: Int,nCoord:Int)(u: Double) = N(self.knots__EquallySpaced)(i,p,nCoord)(u)
        }

      }
      qMatrix

    }

  }
  
   


}
