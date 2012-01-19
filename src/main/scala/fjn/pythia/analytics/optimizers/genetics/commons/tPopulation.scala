package fjn.pythia.analytics.optimizers.genetics.commons

object populationType extends Enumeration {
        type populationType = Value
        val CONTINUOUS,DISCRETE = Value
      }

import populationType._
import collection.mutable.ListBuffer


/**
 * defines the minimum components needed to interact with the set of trials used during
 * an optimization process
 */
class popPair[T] private (key:T,v:Double){
  var chr:T = key;
  var value:Double = v;
 }

object  popPair{
  def create[T](key:T,v:Double):popPair[T]={
   new popPair(key,v);
  }

}


trait tPopulation[T] {

   def init():Unit;
   def getPopulationType:populationType;

   var population:Array[popPair[T]];


   def totalParameterLength:Int;
    /**
   * returns the numberOfSamples best vector with their corresponding fitness as an array of 2-tuple
   */
  def getBest(numberOfSamples:Int):Array[popPair[T]]={

      val res = new ListBuffer[popPair[T]]();
      var filteredPop = population.filter(x=> true);
      for (n <- 0.until(numberOfSamples) if n<population.size )
      {


        val item = filteredPop.max (new Ordering[popPair[T]]{
          def compare(x:popPair[T], y:popPair[T]): Int={
            x.value.compare(y.value)
          }
        })

        res += item;
        filteredPop = filteredPop.filter(x=> x!=item);
        if (filteredPop.length==0)
        {
          return res.toArray;
        }
      }
      res.toArray
  }
   def getWorst():Int={

     try{
           var filteredPop = population.filter(x=> x.value>0);



        val item = filteredPop.min (new Ordering[popPair[T]]{
          def compare(x:popPair[T], y:popPair[T]): Int={
            x.value.compare(y.value)
          }
        })


        population.indexOf(item)


     }catch{
       case _ => -1;
     }


  }

  override def toString():String ={
    val res = new StringBuilder()
    population.foreach(x => res.append( x.value.toString()+" ->"+ x.chr.toString()+";"));

    res.append("\n")
    res.toString()
  }
}