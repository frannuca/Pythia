package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix
import java.util.ArrayList

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 3/23/12
 * Time: 11:03 PM
 * To change this template use File | Settings | File Templates.
 */



trait controlPoints
{
  val qk:Array[Matrix[Double]]
  val dim = qk(0).numberRows
}

trait parameterVector
{
  self:controlPoints =>
  
  //We define the parameter knot location as an array of array of points
  //marking the position of each axis:
  val parameterKnots = new Array[Seq[Double]](self.dim)

  
  //Find the list of points per coordinate:
  
  for (i <- 0 until self.qk.length)
  {
    for (n <- 0 until self.qk(i).numberRows)
    {
      parameterKnots(n) ++ Seq(self.qk(i)(n,0))
    }
      
  }
  
  val orderedParameterKnots = parameterKnots.map(s => s.sortWith((a,b) => a < b))

  
//  def getParameterVector():Array[Matrix[Double]]=
//  {
//    orderedParameterKnots.foreach()
//  }
}
trait KnotsVector
{
  val knots:Array[Double]
}
trait Basis {
  self:KnotsVector =>

  def N(i:Int, p:Int)(u:Double):Double=
  {
    if (p==0)
    {
      if (knots(i) <= u && u < knots(i+1))
        1.0
      else
        0.0
    }
    else
    {
      (u-self.knots(i))/(knots(i+p)-knots(i))*N (i,p-1)(u) +
      (self.knots(i+p+1)-u)/(knots(i+p+1)-knots(i+1))*N (i+1,p-1)(u)  
    }



  }
    
}
