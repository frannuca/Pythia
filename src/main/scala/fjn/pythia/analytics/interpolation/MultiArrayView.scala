package fjn.pythia.analytics.interpolation

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 4/27/12
 * Time: 6:17 PM
 * To change this template use File | Settings | File Templates.
 */

class MultiArrayView[T](storage:Seq[T],dimensions:Seq[Int]) {

  def apply(index:Seq[Int]):T=
  {    
    var index = 0
    for ((n,start) <- index.reverse zip (0 until dimensions.length))
    {
       val restSlices = dimensions.slice(0,dimensions.length-1-1)
       restSlices match{
         case Seq() => index = index + n //the last index only adds
         case _     =>  index = index + n*restSlices.foldLeft(1.0)((acc,v)=> acc*v)
       }

    }
    storage(index)
  }

}
