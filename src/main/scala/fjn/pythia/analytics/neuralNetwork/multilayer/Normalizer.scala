package fjn.pythia.analytics.neuralNetwork.multilayer

import scalala.tensor.dense.DenseMatrix

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 11/25/11
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */

trait Normalizer {
           self : NeuralNetworkBase =>
  var trainingSet: Seq[(DenseMatrix[Double], DenseMatrix[Double])] = null
   var trigger: (Double => Double) = null
   var mu_x: DenseMatrix[Double]= null
   var mu_y: DenseMatrix[Double]= null
   var minLimitX: Double  = 0
   var maxLimitX: Double  = 0
   var minLimitY: Double  = 0
   var maxLimitY: Double = 0
   var maxX: DenseMatrix[Double] = null
   var minX: DenseMatrix[Double]  = null
   var maxY: DenseMatrix[Double] = null
   var minY: DenseMatrix[Double]  = null

  protected def mean(x: Seq[DenseMatrix[Double]], isX:Boolean = true): Unit = {

      var average = DenseMatrix.zeros[Double](x.head.numRows, x.head.numCols)
      var counter = 0;
      x.foreach(input => {
        average += input
        counter += 1
      })

      require(counter > 0)
     if (isX)
     {
      mu_x = average * 1.0 / counter.toDouble
     }
      else
     {
           mu_y = average * 1.0 / counter.toDouble
     }



    }


  private def getMinMax(x: Seq[DenseMatrix[Double]],isX:Boolean = true): Unit = {
    mean(x,isX)
    var mu = mu_x
    if (!isX)
      mu = mu_y

    var auxMaxX = DenseMatrix.zeros[Double](x.head.numRows, 1) - 1e9
    var auxMinX = DenseMatrix.zeros[Double](x.head.numRows, 1) + 1e9




    x.foreach(m2 => {
      val m: DenseMatrix[Double] = m2 - mu
      var k: Int = 0

      while (k < m.numRows) {
        if (auxMaxX(k, 0) < m(k, 0)) auxMaxX(k, 0) = m(k, 0)
        if (auxMinX(k, 0) > m(k, 0)) auxMinX(k, 0) = m(k, 0)
        k += 1
      }

    })

     if(isX)
    {
      maxX = auxMaxX
      minX = auxMinX
    }
    else
    {
      maxY = auxMaxX
      minY = auxMinX
    }
  }

    def initialize(f: (Double => Double), trainingSetv: Seq[(DenseMatrix[Double], DenseMatrix[Double])]): Unit = {
     maxLimitY = f(100d)
     minLimitY = f(-100d)
     minLimitX = -100d;
     maxLimitX = 100d;


     while (f(minLimitX) < minLimitY*0.8  )
       minLimitX += 1e-4

     while (f(maxLimitX) > maxLimitY*0.8 )
       maxLimitX -= 1e-4

     this.trainingSet = trainingSetv

     val xx = trainingSet.map(t => t._1).toArray
     val yy = trainingSet.map(t => t._2).toArray
     mean(xx)
     mean(yy,false)
     getMinMax(xx)
     getMinMax(yy,false)
   }



   def normalizeX(s: DenseMatrix[Double]): DenseMatrix[Double]

   def deNormalizeX(s: DenseMatrix[Double]): DenseMatrix[Double]

   def normalizeY(s: DenseMatrix[Double]): DenseMatrix[Double]

   def deNormalizeY(s: DenseMatrix[Double]): DenseMatrix[Double]
}


trait UnityNormalizer extends Normalizer{
      self:NeuralNetworkBase =>


  def normalizeX(x: DenseMatrix[Double]): DenseMatrix[Double]=
  {
    val s = x.copy - mu_x
        for (j <- 0 until s.numRows) {
          val d = s(j, 0)/maxX(j,0)
          s(j, 0) = d
        }

        s

  }

  def deNormalizeX(x: DenseMatrix[Double]): DenseMatrix[Double]={

    val s = x.copy
        for (j <- 0 until s.numRows) {
          val d = s(j, 0)*maxX(j,0)
          s(j, 0) = d
        }

        s + mu_x
  }

   def normalizeY(x: DenseMatrix[Double]): DenseMatrix[Double]={

     val s = x.copy - mu_y
             for (j <- 0 until s.numRows) {
               val d = s(j, 0)/maxY(j,0)
               s(j, 0) = d
             }

             s


   }

  def deNormalizeY(x: DenseMatrix[Double]): DenseMatrix[Double]={
        val s = x.copy
        for (j <- 0 until s.numRows) {
          val d = s(j, 0)*maxY(j,0)
          s(j, 0) = d
        }

        s + mu_y
  }
}
trait MaxMinNormalizer extends Normalizer {
         self:NeuralNetworkBase =>

  def normalizeX(x: DenseMatrix[Double]): DenseMatrix[Double] = {

    val s = x.copy - mu_x
    for (j <- 0 until s.numRows) {
      val d = (s(j, 0) - minX(j, 0)) / (maxX(j, 0) - minX(j, 0)) * (maxLimitX - minLimitX) + minLimitX
      s(j, 0) = d
    }

    s

  }

  def deNormalizeX(x: DenseMatrix[Double]): DenseMatrix[Double] = {

    val s = x.copy
    for (j <- 0 until s.numRows) {
      val id = (s(j, 0) - minLimitX) / (maxLimitX - minLimitX) * (maxX(j, 0) - minX(j, 0)) + minX(j, 0)
      s(j, 0) = id
    }
   s + mu_x
  }


  def normalizeY(y: DenseMatrix[Double]): DenseMatrix[Double] = {

     val s = y.copy - mu_y
     for (j <- 0 until s.numRows) {
       val d = (s(j, 0) - minY(j, 0)) / (maxY(j, 0) - minY(j, 0)) * (maxLimitY - minLimitY) + minLimitY
       s(j, 0) = d
     }


     s

   }

   def deNormalizeY(y: DenseMatrix[Double]): DenseMatrix[Double] = {

     val s = y.copy
     for (j <- 0 until s.numRows) {
       val id = (s(j, 0) - minLimitY) / (maxLimitY - minLimitY) * (maxY(j, 0) - minY(j, 0)) + minY(j, 0)
       s(j, 0) = id
     }
    s  + mu_y

   }


  //  private def variance(x: Seq[DenseMatrix[Double]]): DenseMatrix[Double] = {
  //
  //    val mu = mean(x)
  //    var counter = 0
  //    var sigma = DenseMatrix.zeros[Double](x.head.numRows, 1)
  //
  //    x.foreach(input => {
  //      val d = (input - mu)
  //      sigma += d * d
  //      counter += 1
  //    })
  //
  //    require(counter > 0)
  //
  //    sigma *= 1.0 / counter.toDouble
  //  }



}