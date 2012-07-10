package fjn.pythia.analytics.interpolation

import org.specs2.mutable.Specification
import fjn.pythia.matrix.Matrix
import fjn.pythia.plotting.plot2D
import scala.collection.JavaConversions._
import java.io.{BufferedReader, InputStreamReader}

import akka.dispatch.Future



/**
 * User: fran
 * Date: 5/7/12
 * Time: 11:15 PM
 */

class testNurbs2D extends Specification {
  "Creating a 2D surface to be interpolated" should {

    "Converge to given solution" in {
      `testAlgorithm` mustEqual true
    }

  }


  def `testAlgorithm` ={

    val nSamplesX=5
    val nSamplesY=15
    val qk =
      (for(
           k<- 0 until nSamplesY;
           h <- 0 until nSamplesX;
        val mt = new Matrix[Double](2,1)
      ) yield {
        mt.zeros;
        mt.set(0,0,3.0*h.toDouble/nSamplesX)
        mt.set(1,0,3.0*k.toDouble/nSamplesY)
        mt
        }
        ).toArray[Matrix[Double]]



    def psinc:Function2[Double,Double,Double] =
      (u,v) => math.sin(math.sqrt(u*u+v*v+1e-4))/math.sqrt(u*u+v*v+1e-4)

    val z =(for(q <- qk)
           yield
            {
              val x= q(0,0)
              val y =q(1,0)

              psinc(x,y)
            }).toArray


    val order =1
    val bspline = new Nurbs2DChord(qk,Array(order,order),Seq(nSamplesX,nSamplesY))
    bspline.solve(z);

      for(item <- qk)
        {
          val u = bspline.getNormalizedCoord( item(0,0),0)
          val v = bspline.getNormalizedCoord( item(1,0),1)
          val ax = bspline(u,v)
          val x = ax(0,0)
          val y = ax(1,0)
          val z = ax(2,0)
          val r = psinc(item(0,0),item(1,0))
          if(math.abs(z-r)>1e-4)
          {
            println("Error in the solved system larger than 1e-3"+"Expected "+r+" obtained "+z)
            failure("error in z")

          }
          else if(math.abs(x-item(0,0))>1e-4)
          {
            println("Error in the solved system larger than 1e-3"+"Expected "+x+" obtained "+item(0,0))
            failure("error in x")
          }
          else if(math.abs(y-item(1,0))>1e-4)
          {
            println("Error in the solved system larger than 1e-3"+"Expected "+y+" obtained "+item(1,0))
            failure("error in y")
          }


        }

    true
  }

}


