package fjn.pythia.analytics.neuralNetwork.multilayer

/**
* Created by IntelliJ IDEA.
* User: fran
* Date: 11/14/11
* Time: 10:24 PM
* To change this template use File | Settings | File Templates.
*/


import org.specs2.mutable.Specification
import scalala.tensor.dense.DenseMatrix
import java.io.{StringWriter, FileOutputStream}

object NNTestUtils{


  def generateXOR():StringBuilder={
    val sbld = new StringBuilder()
    sbld.append(0).append(",").append(0).append(";").append(1).append("\r\n")
    sbld.append(0).append(",").append(1).append(";").append(0).append("\r\n")
    sbld.append(1).append(",").append(0).append(";").append(0).append("\r\n")
    sbld.append(1).append(",").append(1).append(";").append(1).append("\r\n")

    sbld
  }

  def genetateFunc():StringBuilder = {
    val dx=0.1
        val dy = 0.2
        val sbld = new StringBuilder()
        for ( ix <- 0 until 4)
        {
           val x = ix*dx
          for ( iy <- 0 until  4)
          {

            val y = iy*dy
            sbld.append(x).append(",").append(y).append(";").append((30.0*x*x+15.0*y*y)).append("\r\numberOfIteration")

          }

        }

    sbld
  }
  def generateSet1():String = {




    val sbld = generateXOR()
    val writer = new FileOutputStream("C:\\temp\\test.txt")

     writer.write(sbld.toString().getBytes)


    writer.close()

    "C:\\temp\\test.txt"


  }
}


class NN_test extends  Specification {

  "training a NN" should {
    "run" in {
      `testAlgorithm` mustEqual true

    }
  }

  def `testAlgorithm`={



     val nn = new NeuralNetwork(List(2,5,5,1))


    val err = nn.train(NNTestUtils.generateSet1(),true)

    err < 0.01


  }

}