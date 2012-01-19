package fjn.pythia.analytics.neuralNetwork.multilayer

import scalala.tensor.dense.DenseMatrix
import collection.mutable.ListBuffer
import fjn.pythia.analytics.commons.NNMatrixExtensions


/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 11/25/11
 * Time: 6:33 PM
 * To change this template use File | Settings | File Templates.
 */

trait WeightUpdater extends  NNMatrixExtensions   {

  self: NNTrainingCtes =>

  def clearWeights():Unit={
    Ws.clear()
    dWs.clear()
  }
  protected  var Ws = new ListBuffer[DenseMatrix[Double]]
  protected  var dWs = new ListBuffer[DenseMatrix[Double]]

  def resolveWeights():Unit={



     for (i <- 0 until dWs.length){
      applyFunction(x => x * alpha,dWs(i))
      Ws(i) -=  dWs(i)
    }
  }

  def initWeights(size:Seq[Int]):Unit={

    for (n <- 1 until size.length) {
      Ws += (DenseMatrix.rand(size(n - 1) + 1, size(n))-0.5 * 2.0)
      
      //applyFunction(x => x*0.01,Ws.last)
      
      dWs += DenseMatrix.zeros[Double](size(n - 1) + 1, size(n))
    }

    //val dd = math.sqrt ( Ws.head.numRows.toDouble)
    //applyFunction(x => x,Ws.last)
    //applyFunction(x => x/dd,Ws.head)
  }

  def GetWeightCopy():ListBuffer[DenseMatrix[Double]]={
    val cpyW = new ListBuffer[DenseMatrix[Double]]()
    Ws.foreach(w => cpyW+=w.copy)

    cpyW
  }
  def cleardW():Unit={
    dWs.foreach(dw => applyFunction(_=> 0.0,dw))
  }
}

trait WeightUpdaterSimple  extends WeightUpdater {

   self: NNTrainingCtes =>

  type WNet = ListBuffer[DenseMatrix[Double]]
  val timeWindow = new ListBuffer[WNet]


  private def addWNet()
  {
    val w =  dWs.toArray
    val w2 = new ListBuffer[DenseMatrix[Double]]()
    w.foreach(x => w2+=x)

    timeWindow.insert(0,w2)

    if(timeWindow.length>maxWindow){
      while(timeWindow.length > maxWindow)
        timeWindow.remove(timeWindow.length-1)
    }
  }

  override def resolveWeights():Unit={

    super.resolveWeights()
    timeWindow.foreach(wnet =>
    {
      var i = 0;
      while(i<wnet.length)
      {
        wnet(i) = wnet(i).copy* self.beta

        Ws(i)-= wnet(i)
        i+=1
      }
    })

    addWNet()


  }

}




