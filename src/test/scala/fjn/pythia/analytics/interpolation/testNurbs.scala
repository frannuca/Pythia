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
      (for(h <- 0 until 20;
        val mt = new Matrix[Double](2,1)
      ) yield {mt.zeros;val mt2=mt+h.toDouble; mt2}
            ).toArray[Matrix[Double]]
    


    val order = 4
    val bspline = new Nurbs(qk,Array(order,order))



    var xtotal= Seq[java.util.ArrayList[java.lang.Double]]()
    var ytotal= Seq[java.util.ArrayList[java.lang.Double]]()

    xtotal = xtotal ++ Seq(new java.util.ArrayList[java.lang.Double]())
    ytotal = ytotal ++ Seq(new java.util.ArrayList[java.lang.Double]())

    {
      for(i<- 0 until 1000)
          {
            xtotal.last.add(1.0/1000.0 * i.toDouble)
            ytotal.last.add(bspline.NCentripetal(6,order,1)(xtotal.last(i)))

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
