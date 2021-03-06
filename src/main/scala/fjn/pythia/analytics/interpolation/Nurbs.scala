

package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix
import akka.actor.Actor
import akka.actor.Actor._

import collection.mutable.ListBuffer
import akka.dispatch.Future
import fjn.pythia.scheduler.threadpool



/**
 * Created by fjn
 * User: fran
 * Date: 3/23/12
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */

class nurb2DWorker(container:Array[Matrix[Double]],f:(Double,Double)=>Matrix[Double]) extends Actor {
     def receive = {
       case ((u: Double, v: Double,index:Int)) => {
          container(index) = f(u,v)
         true
       }
        case "end" => self.stop()
       case _ =>  false
      }

   }

/**
 *
 * @param qkv: Sequence of 2-component vector containing the samples to be interpolated. The input vector component
 *           distribution is: [0,0]-coordinate = abscissa. [1,0]-coordinate = function value
 *@param basisOrderv: Sequence of 1 value containing the order of the interpolation to be applied
 */
class Nurbs1D(val qkv:Seq[Matrix[Double]],val basisOrderv:Seq[Int])
    extends controlPoints
    with parameterVectorCentripetal
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with Solver1D{

  def qk = qkv
  def basisOrder = basisOrderv
  def dim=Seq(qk.length)

  def  apply(t:Double):Matrix[Double] ={

    var sum = new Matrix[Double](2, 1)
    for (i <- getBasisRange(t)) {
      val pAux = new Matrix[Double](2, 1)
      pAux.set(0, 0, pk(i, 0))
      pAux.set(1, 0, pk(i, 1))
      sum = sum + pAux * NBasis(i, basisOrder(0), 0)(t)

    }

    sum

}



  def getNormalizedCoord(x:Double):Double=
     {

       def nurb:(Double => Double) = x=> {this.apply(x)(0,0)}

       var dLow = 0.0
       var dHigh= 1.0
       var dMean= 0.5

       var maxVal = nurb(dHigh)
       var minVal = nurb(dLow)
       var mean = nurb(dMean)




       var found:Boolean = false
       while(!found)
       {
         if (math.abs(x-mean)<1e-5)
           found = true


         if (x < mean)
           {
             dHigh = dMean
           }
         else if (x>mean)
         {
           dLow = dMean
         }
         else
          found=true

         dMean = (dHigh+dLow)*0.5
         mean = nurb(dMean)
       }

       dMean
     }

  def getBasisRange(t:Double):Seq[Int]={
        val vector = knotsVector(0)
        val sz = vector.length-basisOrder(0)-1

        var i=0
        var found:Boolean=false
        var counter = 0
        while(!found && counter < sz){
            if (t<=vector(counter))
            {
              i = counter
              found=true
            }
          counter = counter + 1
        }

      val resVector =
        if(found){
          i-basisOrder(0)-1 until i+basisOrder(0)+1
        }
        else
          0 until vector.length

       resVector.filter(c => c >= 0 && c < sz)


      }
}

trait Nurbs2DBase
  extends controlPoints
      with BasisFunctionOrder
      with KnotsVector
      with Basis
      with Solver2D{
  self: parameterVector =>


  val xAxis:Seq[Double]
  val yAxis:Seq[Double]

  def qk= for (y <- yAxis;
               x <- xAxis)
      yield
    {
      val m = new Matrix[Double](2,1);
      m.set(0,0,x)
      m.set(1,0,y)
      m
    }
  def tolerance:Double

  def  apply(u:Double,v:Double):Matrix[Double] ={

        var sum = new Matrix[Double](3,1)
        for (i <- getBasisRange(0)(u) )
        {
          for (j <-  getBasisRange(1)(v))
          {
            val pAux = new Matrix[Double](3,1)
            pAux.set(0,0,pk(i)(j,0))
            pAux.set(1,0,pk(i)(j,1))
            pAux.set(2,0,pk(i)(j,2))
            val basis = (NBasis(i,basisOrder(0),0)(u)*NBasis(j,basisOrder(1),1)(v))
            sum = sum +    pAux * basis
          }

        }

      sum
  }


  /**
   * Multithreaded calculation
   * @param uv list of pair coordinates
   * @return result interpolated sequence of results
   */
  def  apply(uv:Seq[(Double,Double)]):Seq[Matrix[Double]] ={

    println("Starting Batch computation of points")
          def func = (u:Double,v:Double) =>
          {
            this.apply(u,v)
          }

          val futures =
          for ( item <- uv)
            yield
         {
            ()=>func(item._1,item._2)

         }
    new threadpool(futures,4,10000).run()
  }

  def getNormalizedCoord(x:Double,nCoord:Int):Double=
       {

         def nurb:(Double => Double) = x=> {if (nCoord==0) this.apply(x,0)(nCoord,0) else this.apply(0,x)(nCoord,0)}

         var dLow = 0.0
         var dHigh= 1.0
         var dMean= 0.5


         var mean = nurb(dMean)
         var counter = 0
         var found:Boolean = false
         while(!found)
         {
           if (math.abs(x-mean) < tolerance)
           {
             found = true
           }
             else
           {
             if (x < mean)
            {
              dHigh = dMean
            }
            else if (x>mean)
            {
              dLow = dMean
            }
            else
             found=true

            dMean = (dHigh+dLow)*0.5
            mean = nurb(dMean)

            if(dHigh<=dLow)
              found=true
            }
           counter =counter + 1
            if(counter>500)
              found=true
           }



         dMean
       }

  def getBasisRange(nCoord:Int)(t:Double):Seq[Int]={
      val vector = knotsVector(nCoord)
      val sz = vector.length-basisOrder(nCoord)-1

      var i=0
      var found:Boolean=false
      var counter = 0
      while(!found && counter < sz){
          if (t<=vector(counter))
          {
            i = counter
            found=true
          }
        counter = counter + 1
      }

    val resVector =
      if(found){
        i-basisOrder(nCoord)-1 until i+basisOrder(nCoord)+1
      }
      else
        0 until vector.length

     resVector.filter(c => c >= 0 && c < sz)


    }
}


class Nurbs2D(val xAxis:Seq[Double],val yAxis:Seq[Double],val basisOrderv:Array[Int],val dimv:Seq[Int],implicit val tolerance:Double=1.0e-4)
  extends Nurbs2DBase  with parameterVectorEquallySpaced  {


  def basisOrder = basisOrderv
  def dim = dimv


  override def getNormalizedCoord(x:Double,nCoord:Int):Double=
     {


       def nurb:(Double => Double) = x=> {if (nCoord==0) this.apply(x,0)(nCoord,0) else this.apply(0,x)(nCoord,0)}

       var dLow = 0.0
       var dHigh= 1.0
       var dMean= 0.5

       var maxVal = nurb(dHigh)
       var minVal = nurb(dLow)

       dMean = (x-minVal)/(maxVal-minVal)
       dMean

     }
}


class Nurbs2DChord(val xAxis:Seq[Double],val yAxis:Seq[Double],val basisOrderv:Array[Int],val dimv:Seq[Int],implicit val tolerance:Double=1.0e-4)
  extends  parameterVectorChord with Nurbs2DBase{


    def basisOrder = basisOrderv
    def dim = dimv

  println("Nurbs2DChord")

}


class Nurbs2DCentripetal(val xAxis:Seq[Double],val yAxis:Seq[Double],val basisOrderv:Array[Int],val dimv:Seq[Int],implicit val tolerance:Double=1.0e-4)
  extends controlPoints with parameterVectorCentripetal with Nurbs2DBase{

    def basisOrder = basisOrderv
    def dim = dimv


  println("Nurbs2DCentripetal")

}
