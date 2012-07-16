package fjn.pythia.analytics.interpolation

import org.specs2.mutable.Specification
import fjn.pythia.matrix.Matrix
import fjn.pythia.plotting.plot2D
import scala.collection.JavaConversions._
import java.io.{BufferedReader, InputStreamReader}

import akka.dispatch.Future

/**
 * Created by fjn army of one.
 * User: fran
 * Date: 7/11/12
 * Time: 8:39 PM
 */

class testArbitraryNurbs  extends Specification {
  "Creating a 2D surface to be interpolated" should {

    "Converge to given solution" in {
      `testAlgorithm` mustEqual true
    }

  }


  def `testAlgorithm` ={

    def psinc:Function2[Double,Double,Double] =
          (u,v) => math.sin(math.sqrt(u*u+v*v+1e-4))/math.sqrt(u*u+v*v+1e-4)
    val nSamplesX=5
    val nSamplesY=5
    val qk =
      (for(k<- 0 until nSamplesY )
        yield
      {
        val y =3.0*k.toDouble/nSamplesY
        (for (h <- 0 until nSamplesX)
          yield
        {
          val mt = new Matrix[Double](3,1)
          mt.zeros;
          val x = 3.0*h.toDouble/nSamplesX

                  mt.set(0,0,x)
                  mt.set(1,0,y)
                  mt.set(2,0,psinc(x,y))
                  mt
        }).toSeq

        }
        ).toSeq



    val order =2
    val bspline = new ArbitraryNurbs2D(qk,Array(order,order))


      for(items <- qk;
          item <- items)
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


