

package fjn.pythia.analytics.interpolation

import fjn.pythia.matrix.Matrix



/**
 * Created by fjn
 * User: fran
 * Date: 3/23/12
 * Time: 6:38 PM
 * To change this template use File | Settings | File Templates.
 */


class Nurbs1D(val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int])
    extends controlPoints
    with parameterVectorCentripetal
    with BasisFunctionOrder
    with KnotsVector
    with Basis
    with Solver1D{
  def  apply(t:Double):Matrix[Double] ={

    var sum = new Matrix[Double](2, 1)
    for (i <- getBasisRange(t)) {
      val pAux = new Matrix[Double](2, 1);
      pAux.set(0, 0, pk(i, 0))
      pAux.set(1, 0, pk(i, 1))
      sum = sum + pAux * NBasis(i, basisOrder(0), 0)(t)

    }

    sum

}


  def getNormalizedCoord(x:Double):Double=
     {

       def nurb:(Double => Double) = x=> {this.apply(x)(0,0)}

       var dLow = 0.0
       var dHigh= 1.0
       var dMean= 0.5

       var maxVal = nurb(dHigh)
       var minVal = nurb(dLow)
       var mean = nurb(dMean)




       var found:Boolean = false
       while(!found)
       {
         if (math.abs(x-mean)<1e-3)
           found = true


         if (x<mean)
           {
             dHigh = dMean
           }
         else if (x>mean)
         {
           dLow = dMean
         }
         else
          found=true

         dMean = (dHigh+dLow)*0.5
         mean = nurb(dMean)
       }

       dMean
     }

  def getBasisRange(t:Double):Seq[Int]={
        val vector = knotsVector(0)
        val sz = vector.length-basisOrder(0)-1;

        var i=0
        var found:Boolean=false
        var counter = 0
        while(!found && counter < sz){
            if (t<=vector(counter))
            {
              i = counter;
              found=true;
            }
          counter = counter + 1
        }

      val resVector =
        if(found){
          i-basisOrder(0)-1 until i+basisOrder(0)+1
        }
        else
          0 until vector.length

       resVector.filter(c => c>=0 && c<sz)


      }
}


class Nurbs2D(val qk:Array[Matrix[Double]],val basisOrder:Array[Int],val dim:Seq[Int])
  extends controlPoints
      with parameterVectorEquallySpaced
      with BasisFunctionOrder
      with KnotsVector
      with Basis
      with Solver2D{

  def  apply(u:Double,v:Double):Matrix[Double] ={

        var sum = new Matrix[Double](3,1)
        for (i <- getBasisRange(0)(u) )
        {
          for (j <-  getBasisRange(1)(v))
          {
            val pAux = new Matrix[Double](3,1);
            pAux.set(0,0,pk(i)(j,0))
            pAux.set(1,0,pk(i)(j,1))
            pAux.set(2,0,pk(i)(j,2))
            val basis = (NBasis(i,basisOrder(0),0)(u)*NBasis(j,basisOrder(1),1)(v))
            sum = sum +    pAux * basis
          }

        }

      sum
  }
  
  def getNormalizedCoord(x:Double,nCoord:Int):Double=
   {
     
     def nurb:(Double => Double) = x=> {if (nCoord==0) this.apply(x,0)(nCoord,0) else this.apply(0,x)(nCoord,0)}
     
     var dLow = 0.0
     var dHigh= 1.0
     var dMean= 0.5
       
     var maxVal = nurb(dHigh)
     var minVal = nurb(dLow)
     var mean = nurb(dMean)
     
     
     
     
     var found:Boolean = false
     while(!found)
     {
       if (math.abs(x-mean)<1e-3)
         found = true

       
       if (x<mean)
         {
           dHigh = dMean
         }
       else if (x>mean)
       {
         dLow = dMean
       }
       else
        found=true

       dMean = (dHigh+dLow)*0.5
       mean = nurb(dMean)
     }

     dMean
   }

  def getBasisRange(nCoord:Int)(t:Double):Seq[Int]={
      val vector = knotsVector(nCoord)
      val sz = vector.length-basisOrder(nCoord)-1;

      var i=0
      var found:Boolean=false
      var counter = 0
      while(!found && counter < sz){
          if (t<=vector(counter))
          {
            i = counter;
            found=true;
          }
        counter = counter + 1
      }        
    
    val resVector =
      if(found){
        i-basisOrder(nCoord)-1 until i+basisOrder(nCoord)+1 
      }
      else
        0 until vector.length
    
     resVector.filter(c => c>=0 && c<sz)
    
      
    }
}

