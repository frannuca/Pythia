package fjn.pythia.analytics.interpolation

import org.specs2.mutable.Specification
import fjn.pythia.matrix.Matrix
import fjn.pythia.plotting.plot2D
import scala.collection.JavaConversions._
import java.io.{BufferedReader, InputStreamReader}

import akka.dispatch.Future
import fjn.pythia.analytics.interpolation.Nurbs


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/7/12
 * Time: 11:15 PM
 * To change this template use File | Settings | File Templates.
 */

class testNurbs2D extends Specification {
  "Creating a optimization test for SWARM" should {

    "Converge to given solution" in {
      `testAlgorithm` mustEqual true
    }

  }


  def `testAlgorithm` ={

    val nSamples=50
    val qk =
      (for(h <- 0 until nSamples;
           k<- 0 until nSamples;
        val mt = new Matrix[Double](2,1)
      ) yield {
        mt.zeros;
        mt.set(0,0,k.toDouble/nSamples.toDouble*3.1415*20.0)
        mt.set(1,0,h.toDouble/nSamples.toDouble*3.1415*20.0)
        mt
        }
        ).toArray[Matrix[Double]]



    val z =(for(q <- qk)
           yield
            {
              val x= q(0,0)
              val y =q(1,0)

              math.sin(3.1415*x)/(3.1415*x) * math.sin(3.1415*y)/(3.1415*y)
            }).toArray


    val order =3
    val bspline = new Nurbs(qk,Array(order,order),Seq(nSamples,nSamples))
    bspline.solve(z);


    val ax = bspline.NCentripetal(4,order,0)(1)


    var xtotal= Seq[java.util.ArrayList[java.lang.Double]]()
    var ytotal= Seq[java.util.ArrayList[java.lang.Double]]()
//
//
//
//
//      for(nf <- 0 until nSamples )
//        {
//
//          xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
//          ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
//
//          for(i<- 0 until  1001)
//              {
//                val t = 1.0/ 1000.toDouble * i.toDouble
//                val ax = bspline.NCentripetal(nf,order,0)(t)
//                //ax.set(1,0,z(i))
//
//                xtotal.last.add(t)
//                ytotal.last.add(ax)
//
//              }
//
//        }
    xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
    ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())

    {
      for(i<- 0 until  1000)
      {
        val t = 1.0/ 1000.toDouble * i.toDouble
        val ax = bspline(t)
        // ax.set(1,0,z(i))

        xtotal.last.add(ax(0,0))
        ytotal.last.add(ax(1,0))

      }



    }

    xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
    ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())

    {
      for(i<- 0 until  z.length)
      {



        xtotal.last.add(qk(i)(0,0))
        ytotal.last.add(z(i))

      }



    }
//
//    val axtest = bspline(1.0)

    for (i <- 0 until xtotal.length)
    {
      val p = new plot2D()
      p.AddCurve(xtotal(i),ytotal(i),i.toString);
      val fut =
        Future
              {
                p.showPanel()

              }

    }

    val br = new BufferedReader(new InputStreamReader(System.in));
    br.readLine()



    true
  }

}


