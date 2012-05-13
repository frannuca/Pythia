package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix

/**
 * Created by fjn
 * User: fran
 * Date: 5/13/12
 * Time: 3:27 PM
 * To change this template use File | Settings | File Templates.
 */

/**
 * This trait computes the location of the control points into the target nurbs space
 */
trait parameterVector {
  self: controlPoints =>

  //Extracting the positions of each axis:
  protected val parameterKnotsAux =
    (for (i <- 0 until self.dim.length) yield Seq[Double]()).toArray


  //Find the list of points per coordinate:
  for ((sz, nCoord) <- self.dim zip (0 until self.dim.length)) {
    for (i <- 0 until sz) {

      val m = (for (j <- 0 until self.dim.length) yield 0).toArray[Int]
      m(nCoord) = i

      parameterKnotsAux(nCoord) = parameterKnotsAux(nCoord) ++ Seq(viewer(m)(nCoord, 0))
    }
  }
 
  //Calculating the final parameter knots associated to the sequence of points qk
  // with the Chord method
  val parameterKnots:Array[Seq[Double]]


  val tqk:Array[Matrix[Double]]

  }



trait parameterVectorCentripetal extends parameterVector {
  self: controlPoints =>


  val parameterKnots = (for (i <- 0 until parameterKnotsAux.length) yield Seq[Double]()).toArray

 for (i <- 0 until parameterKnotsAux.length){

   //Calculate the sum of the sqrt differences between samples per coordinate for the Centripetal
         //distribution normalization
         val sqrt_normLength =
           (for (n <- 1 until parameterKnotsAux(i).length;
                 val d = math.sqrt(parameterKnotsAux(i)(n) - parameterKnotsAux(i)(n - 1))
           ) yield d).toList.foldLeft(0.0)((acc, v) => acc + v)

  parameterKnots(i) = parameterKnots(i) ++ Seq(0.0)

  for (n <- 1 until parameterKnotsAux(i).length - 1) {

    parameterKnots(i) =
      parameterKnots(i) ++ Seq(parameterKnots(i)(n - 1) + math.sqrt(math.abs(parameterKnotsAux(i)(n) - parameterKnotsAux(i)(n - 1))) / sqrt_normLength)
  }

  parameterKnots(i) =
    parameterKnots(i) ++ Seq(1.0d)



 }
  val tqk =
          (for (i <- 0 until qk.length) yield {
            val q: Matrix[Double] = qk(i).clone()
            val xy = viewer.fromIndex2Seq(i)
            for ((n, v) <- (0 until xy.length) zip xy) {
              q.set(n, 0, parameterKnots(n)(v))
            }
            q
          }).toArray[Matrix[Double]]

}


trait parameterVectorChord extends parameterVector {
  self: controlPoints =>


 

  val parameterKnots = (for (i <- 0 until parameterKnotsAux.length) yield Seq[Double]()).toArray


  for (i<- 0 until parameterKnotsAux.length){
   //Calculate the sum of the differences between samples per coordinate for the Chord distribution
      //normalization
      val normLength =
        (for (n <- 1 until parameterKnotsAux(i).length;
              val d = parameterKnotsAux(i)(n) - parameterKnotsAux(i)(n - 1)
        ) yield d).toList.foldLeft(0.0)((acc, v) => acc + v)
  
  parameterKnots(i) = parameterKnots(i) ++ Seq(0.0)

  for (n <- 1 until parameterKnotsAux(i).length - 1) {

    parameterKnots(i) =
      parameterKnots(i) ++ Seq(parameterKnots(i)(n - 1) + math.sqrt(math.abs(parameterKnotsAux(i)(n) - parameterKnotsAux(i)(n - 1))) / normLength)
  }

  parameterKnots(i) =
    parameterKnots(i) ++ Seq(1.0d)


  }

  val tqk =
          (for (i <- 0 until qk.length) yield {
            val q: Matrix[Double] = qk(i).clone()
            val xy = viewer.fromIndex2Seq(i)
            for ((n, v) <- (0 until xy.length) zip xy) {
              q.set(n, 0, parameterKnots(n)(v))
            }
            q
          }).toArray[Matrix[Double]]

}

trait parameterVectorEquallySpaced extends parameterVector {
  self: controlPoints =>


  

  val parameterKnots = (for (i <- 0 until parameterKnotsAux.length) yield Seq[Double]()).toArray


  for (i <- 0 until parameterKnotsAux.length){
    //Calculate the sum of the differences between samples per coordinate for the Chord distribution
          //normalization
          val normLength =
            (for (n <- 1 until parameterKnotsAux(i).length;
                  val d = parameterKnotsAux(i)(n) - parameterKnotsAux(i)(n - 1)
            ) yield d).toList.foldLeft(0.0)((acc, v) => acc + v)


  parameterKnots(i) = parameterKnots(i) ++ Seq(0.0)

  for (n <- 1 until parameterKnotsAux(i).length - 1) {

    parameterKnots(i) =
      parameterKnots(i) ++ Seq(n.toDouble / (parameterKnotsAux(i).length - 1).toDouble)
  }

  parameterKnots(i) =
    parameterKnots(i) ++ Seq(1.0d)
  }

  val tqk =
        (for (i <- 0 until qk.length) yield {
          val q: Matrix[Double] = qk(i).clone()
          val xy = viewer.fromIndex2Seq(i)
          for ((n, v) <- (0 until xy.length) zip xy) {
            q.set(n, 0, parameterKnots(n)(v))
          }
          q
        }).toArray[Matrix[Double]]

}
