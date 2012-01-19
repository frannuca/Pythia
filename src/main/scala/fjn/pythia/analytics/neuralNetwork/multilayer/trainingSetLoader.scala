package fjn.pythia.analytics.neuralNetwork.multilayer

import scalala.tensor.dense.DenseMatrix
import java.io._
import collection.mutable.ListBuffer
import org.slf4j.LoggerFactory

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 11/29/11
 * Time: 7:33 PM
 * To change this template use File | Settings | File Templates.
 */

trait  trainingSetLoader {

  def logger = LoggerFactory.getLogger(classOf[trainingSetLoader])
  def LoadTrainningSet(filename:String):Option[Seq[(DenseMatrix[Double], DenseMatrix[Double])]] ={


    try
    {
      val resultList = new ListBuffer [(DenseMatrix[Double],DenseMatrix[Double])]
          val dataStr = getDataFromFile(filename) match{
            case(Right(str)) => str
            case(Left(e)) =>  logger.error("getDataFromFile",e); return None
          }
          val rows:Array[String] = dataStr.split('\n')

          for (row <- rows) {

            val inout = row.split(";")
            val inputs = inout(0).split(",")
            val outputs = inout(1).split(",")

            val input = DenseMatrix.zeros[Double](inputs.length,1)
            for (i <- 0 until input.numRows)
               input(i,0)= inputs(i).toDouble

            val output =  DenseMatrix.zeros[Double](outputs.length,1)
            for (i <- 0 until output.numRows)
            {
              output(i,0)=outputs(i).toDouble
            }

            resultList+= Tuple2(input,output)

          }

          Some(resultList.toSeq)

    }
    catch {
      case(e:Exception)=> logger.error("Train loader",e);  None;
      case _ => logger.error("unknown error in Train loader"); None;
    }



  }

  private def getDataFromFile(filename:String):Either[Exception,StringBuilder]={

    try
    {
     val  in = new BufferedReader(new FileReader(filename))

    val sb = new StringBuilder()
    var s:String = in.readLine()
    while(s != null)
    {
      sb.append(s + "\n")
      s = in.readLine()
    }

    in.close();
    return Right(sb)
    }
    catch{
      case(ioe:IOException) => Left(ioe)
      case (e:Exception) => Left(e)
      case _ => Left(new Exception("Unknown exception"))
    }

  }

}