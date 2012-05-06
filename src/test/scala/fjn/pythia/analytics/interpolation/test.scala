package fjn.pythia.analytics.interpolation

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 4/27/12
 * Time: 6:17 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.collection.JavaConversions._
import java.io.{BufferedReader, InputStreamReader}



import org.specs2.mutable.Specification

class test  extends Specification {
    "Creating indexing access" should {

      "Converge to given solution" in {
        check mustEqual true
      }

    }
  def check:Boolean= {
    val sq = (0 until 9).toSeq.map(x => x.asInstanceOf[Double])
    val dim = List(3, 3)
     var ok = true
    val mvw = new MultiArrayView(sq, dim)

    var counter = 0
    for (i <- 0 until dim(0) if ok) {
      for (j <- 0 until dim(1) if ok) {
        val a = mvw(Seq(j, i))
        val index = mvw.fromIndex2Seq(counter)
        counter = counter + 1
       ok =  (i==index(1) && j==index(0))
      }
    }
    ok

  }

}
