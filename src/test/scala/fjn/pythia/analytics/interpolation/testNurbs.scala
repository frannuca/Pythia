package fjn.pythia.analytics.interpolation

import org.specs2.mutable.Specification
import fjn.pythia.matrix.Matrix
import fjn.pythia.plotting.plot2D
import scala.collection.JavaConversions._
import java.io.{BufferedReader, InputStreamReader}

import akka.actor.Actor
import akka.actor.Actor.{actorOf}
import akka.event.EventHandler
import java.util.ArrayList
import akka.dispatch.Future
import fjn.pythia.analytics.interpolation.Nurbs

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/29/12
 * Time: 8:24 PM
 * To change this template use File | Settings | File Templates.
 */

class testNurbs  extends Specification {
  "Creating a optimization test for SWARM" should {

    "Converge to given solution" in {
      `testAlgorithm` mustEqual true
    }

  }

  
  def `testAlgorithm` ={

    val nSamples=50
    val qk =
      (for(h <- 0 until nSamples;
        val mt = new Matrix[Double](1,1)
      ) yield {mt.zeros;val mt2=mt+(h.toDouble/nSamples.toDouble*3.1415*20.0); mt2}
            ).toArray[Matrix[Double]]
    


    val z =(for(h <- 0 until nSamples;
            val r = math.cos(h.toDouble/nSamples.toDouble*3.1415*20.0)*math.exp(-h.toDouble/nSamples)) yield r).toArray[Double]

      val order =3
    val bspline = new Nurbs(qk,Array(order,order))
    bspline.solve(z);


    val ax = bspline.NCentripetal(4,order,0)(1)


    var xtotal= Seq[java.util.ArrayList[java.lang.Double]]()
    var ytotal= Seq[java.util.ArrayList[java.lang.Double]]()




      for(nf <- 0 until nSamples )
        {

          xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
          ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())

          for(i<- 0 until  1001)
              {
                val t = 1.0/ 1000.toDouble * i.toDouble
                val ax = bspline.NCentripetal(nf,order,0)(t)
                //ax.set(1,0,z(i))

                xtotal.last.add(t)
                ytotal.last.add(ax)

              }

        }
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
