package fjn.pythia.analytics.interpolation

import java.util.Arrays.ArrayList



class MultiArrayView[T](storage:Seq[T],dimensions:Seq[Int]) {

   
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


