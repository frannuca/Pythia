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
    val qk =
      (for(h <- 0 until 8;
        val mt = new Matrix[Double](1,1)
      ) yield {mt.zeros;val mt2=mt+h.toDouble; mt2}
            ).toArray[Matrix[Double]]
    


    val z =(for(h <- 0 until 8;
            val r = math.cos(h.toDouble/8.0*3.1415*2.0)* math.cos(h.toDouble/8.0*3.1415*2.0)) yield r).toArray[Double]

    val order = 3
    val bspline = new Nurbs(qk,Array(order,order))
    bspline.solve(z);



    var xtotal= Seq[java.util.ArrayList[java.lang.Double]]()
    var ytotal= Seq[java.util.ArrayList[java.lang.Double]]()

    xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
    ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())

    {
      for(i<- 0 until 1001)
          {
            val t = 1.0/1000.0 * i.toDouble
            val ax = bspline(t)
            xtotal.last.add(ax(0,0))
            ytotal.last.add(ax(1,0))

          }

    }


    for (i <- 0 until xtotal.length)
    {
      val fut =
        Future
              {  val p = new plot2D();
                 p.Show(xtotal(i),ytotal(i),i.toString);
              }

    }

    val br = new BufferedReader(new InputStreamReader(System.in));
    br.readLine()



    true
  }

}
