package fjn.pythia.analytics.interpolation

import org.specs2.mutable.Specification
import fjn.pythia.matrix.Matrix
import fjn.pythia.plotting.plot2D
import scala.collection.JavaConversions._
import java.io.{BufferedReader, InputStreamReader}

import akka.dispatch.Future



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

    val nSamples=5
    val qk =
      (for(h <- 0 until nSamples;
           k<- 0 until nSamples;
        val mt = new Matrix[Double](2,1)
      ) yield {
        mt.zeros;
        mt.set(1,0,h.toDouble)
        mt.set(0,0,k.toDouble)
        mt
        }
        ).toArray[Matrix[Double]]



    val z =(for(q <- qk)
           yield
            {
              val x= q(0,0)
              val y =q(1,0)

              x+y
            }).toArray


    val order =1
    val bspline = new Nurbs2D(qk,Array(order,order),Seq(nSamples,nSamples))
    bspline.solve(z);

    val rng1 = bspline.getBasisRange(0)(0.5);



 //

    var xtotal= Seq[java.util.ArrayList[java.lang.Double]]()
    var ytotal= Seq[java.util.ArrayList[java.lang.Double]]()

//
//
//
      for(nf <- 0 until nSamples )
        {

          xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
          ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())


          for(i<- 0 until  10)
              {
                for(j<- 0 until  10)
                {
                  val u = 1.0/ 10.toDouble * i.toDouble
                  val v = 1.0/ 10.toDouble * j.toDouble
                  val ax = bspline(u,v)
                  val b=1.0
                                  //ax.set(1,0,z(i))

//                                  xtotal.last.add(t)
//                                  ytotal.last.add(ax)
                }


              }

        }
        var aa = bspline(1,1)
        aa = bspline(0.25,0.25)
        aa = bspline(0.75,0.25)
        val aaa=0
     for(h <- 0 until 11)
     {
       var u1 = bspline.getNormalizedCoord(h.toDouble,0)
       println(h.toString + "->"+u1.toString)
     }
       

//    xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
//    ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
//
//    {
//      for(i<- 0 until  1000)
//      {
//        val t = 1.0/ 1000.toDouble * i.toDouble
//        val ax = bspline(t)
//        // ax.set(1,0,z(i))
//
//        xtotal.last.add(ax(0,0))
//        ytotal.last.add(ax(1,0))
//
//      }
//
//
//
//    }

//    xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
//    ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
//
//    {
//      for(i<- 0 until  z.length)
//      {
//
//
//
//        xtotal.last.add(qk(i)(0,0))
//        ytotal.last.add(z(i))
//
//      }
//
//
//
//    }
//
//    val axtest = bspline(1.0)

//    for (i <- 0 until xtotal.length)
//    {
//      val p = new plot2D()
//      p.AddCurve(xtotal(i),ytotal(i),i.toString);
//      val fut =
//        Future
//              {
//                p.showPanel()
//
//              }
//
//    }

    val br = new BufferedReader(new InputStreamReader(System.in));
    br.readLine()



    true
  }

}


