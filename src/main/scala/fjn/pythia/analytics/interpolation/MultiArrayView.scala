package fjn.pythia.analytics.interpolation

import java.util.Arrays.ArrayList


/**
 * Helper class to facilitate the access to linear vector of matrices representing 2D grids
 * @param storage: sequence of  items representing a n-D grid structure
 * @param dimensions number of samples per dimension. Each dimension in the grid contains an homogeneous number of samples
 *                   per slice. This information is passed through this parameter
 * @tparam T: Type of item. Typically we will have double or Matrix[Double] types
 */
class MultiArrayView[T](storage:Seq[T],dimensions:Seq[Int]) {

  /**
   * Given the sequence of coordinates returns the item at this location.
    * @param index: coordinates as a seq of integers
   * @return item located at this position
   */
  def apply(index:Seq[Int]):T=
  {    
    var d = 0
    for ((n,start) <- (index).reverse zip (0 until dimensions.length))
    {
       val restSlices = dimensions.slice(0,dimensions.length-1-start)
       restSlices match{
         case Seq() => d = d + n //the last index only adds
         case _     =>  d = d + n*restSlices.foldLeft(1)((acc,v)=> acc*v)
       }

    }
    storage(d)
  }

  /**
   * computes the coordinates of a specific position in the storage array
   * @param nCoord index position in the storage array
   * @return coordinate position as a sequence of int.
   */
  def fromIndex2Seq(nCoord:Int):Seq[Int]=
  {

    var v=nCoord
        (for (start <- 0 until dimensions.length)
        yield
        {
           val restSlices = dimensions.slice(0,dimensions.length-1-start)
           restSlices match{
             case Seq() => v
             case _     =>  
             {
               val auxSize = (restSlices.foldLeft(1)((acc,v)=> acc*v))
               val aux = math.floor( v/(auxSize) ).toInt
               v = v -aux*auxSize
               aux
             }
           }

        }).toSeq.reverse
  }

}


