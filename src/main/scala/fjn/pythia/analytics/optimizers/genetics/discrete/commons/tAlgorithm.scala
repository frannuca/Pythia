package fjn.pythia.analytics.optimizers.genetics.discrete.commons

import collection.mutable.ListBuffer


trait tAlgorithm{

  /**
   * Evolution of the tAlgorithm is performed through calls to next
   */
   def next():Boolean;





  /**
   * Fitness/Error function to be used for optimization
   */
  val pFitness:(Array[Double]) => Double

  /**
   * Calls the next function  iteration number of times.
   * This method is the only one to be used from the class user side
   * After running the total number of iterations return with the best parameter vector and Fitness
   * obtained till now.
   */
  def +=(iteration:Int)={

    var keepGoing = true;
    for (i<- 0.until(iteration) if(keepGoing)) {
      keepGoing = next()
      iterations += 1;

    }

  }

  var iterations:Int = 0;


}